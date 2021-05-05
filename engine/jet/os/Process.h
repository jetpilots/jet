// system() call that does posix_spawnp (or fork/exec directly)
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <spawn.h>
#include <sys/wait.h>

extern char** environ;

void test_fork_exec(void);
void test_posix_spawn(void);

/*
void test_fork_exec(void)
{
    pid_t pid;
    int status;
    puts("Testing fork/exec");
    fflush(NULL);
    pid = fork();
    switch (pid) {
    case -1:
        perror("fork");
        break;
    case 0:
        execl("/bin/lse", "ls", (char*)0);
        perror("exec");
        break;
    default:
        printf("Child id: %i\n", pid);
        fflush(NULL);
        if (waitpid(pid, &status, 0) != -1) {
            printf("Child exited with status %i\n", status);
        } else {
            perror("waitpid");
        }
        break;
    }
}
*/

// API functions:
// launch launches the given cmdline and returns pid.
// shell is like launch but prepends "/bin/sh -c ..." so you can get
//    sh processing on the cmdline.
// update queries (waitpid w/ WNOHANG) and checks if process is done.
// await calls waitpid on the pid w/o WNOHANG and blocks until done.

typedef struct {
    // max pid is 0x400000, signed int
    // https://stackoverflow.com/questions/6294133/maximum-pid-in-linux
    // http://web.archive.org/web/20111209081734/http://research.cs.wisc.edu/condor/condorg/linux_scalability.html
    // https://utcc.utoronto.ca/~cks/space/blog/unix/PidRollover?showcomments
    // https://unix.stackexchange.com/questions/16883/what-is-the-maximum-value-of-the-process-id
    int pid : 23, exited : 1, code : 8;
    // would love to have a flag for signaled/stopped, but we are at 32 bits
} Process;

typedef struct {
    Process proc;
    // short int fd[2]; // OR you can have 3 ints and make it 16B total
    int p_read, p_write, p_err; //, p_out, p_err;
    // int a, b, c;
} PipedProcess;

// should also have a pipe
// https://stackoverflow.com/questions/7292642/grabbing-output-from-exec
// https://stackoverflow.com/questions/8189935/is-there-any-way-to-ping-a-specific-ip-address-with-c?noredirect=1&lq=1
// ping example shows pipe.
// speaking of ping, you should have a function ping

Process launch(char* args[]) {
    pid_t pid;
    // spawnp won't allow for creating a pipe.
    int ret = posix_spawnp(&pid, args[0], NULL, NULL, args, environ);
    if (ret) printf("error spawning '%s': %s\n", args[0], strerror(ret));
    return (Process) { .pid = ret ? 0 : pid };
}

// I guess the only need for a pipe is to actually read the output as a string.
// or iterate over lines and throw away the string (more rarely).
// redirecting output to a file can always be done by shlaunch("... > outfile")
// so no special func is provided.
typedef enum {
    JET_PIPE_NONE = 0,
    JET_PIPE_READ = 1,
    JET_PIPE_WRITE = 2,
    JET_PIPE_READERR = 4
} PipedProcessCapture;

PipedProcess ppipe(char* args[], int capture) {
    int p_from[2] = { -1, -1 };
    int p_to[2] = { -1, -1 };
    int p_errfrom[2] = { -1, -1 }; // from parent to child

    if (capture & JET_PIPE_READ) pipe(p_from);
    if (capture & JET_PIPE_WRITE) pipe(p_to);
    if (capture & JET_PIPE_READERR) pipe(p_errfrom);

    pid_t pid = fork();
    if (pid == -1) {
        printf("error in fork()\n");
    } else if (pid == 0) {
        if (capture & JET_PIPE_READ) {
            dup2(p_from[1], STDOUT_FILENO);
            close(p_from[1]);
        }
        // parent will get it on p_from[0]
        if (capture & JET_PIPE_WRITE) {
            dup2(p_to[0], STDIN_FILENO);
            close(p_to[0]);
        }
        // parent will send it to p_to[1]
        if (capture & JET_PIPE_READERR) {
            dup2(p_errfrom[1], STDERR_FILENO);
            close(p_errfrom[1]);
        }

        close(p_from[0]);
        close(p_errfrom[0]);
        close(p_to[1]);

        execvp(args[0], args);
        close(p_from[1]);
        close(p_errfrom[1]);
        close(p_to[0]);
        _exit(99);
    }

    close(p_from[1]);
    close(p_errfrom[1]);
    close(p_to[0]);

    PipedProcess pproc = { //
        .proc = (Process) { .pid = pid },
        .p_read = p_from[0],
        .p_err = p_errfrom[0],
        .p_write = p_to[1]
    };

    return pproc;

    // you can do read(pp.p_read, buf, bufsize), write(pp.p_write, buf, bufsize)
    // then close(pp)
}
char wrote(int fd, void* data, unsigned int size) {
    return write(fd, data, size) == size;
}
void pwrite(PipedProcess proc, void* data, ssize_t size) {
    static const unsigned int maxsz = 1 << 30;
    do {
        ssize_t sz = size > maxsz ? maxsz : size;
        if (!wrote(proc.p_write, data, sz)) {
            // deal with errro
            printf("err: write\n");
        }
        size -= sz;
    } while (size > 0);
}

void close(PipedProcess* proc) {
    close(proc->p_read), close(proc->p_write), close(proc->p_err);
    proc->p_read = proc->p_write = proc->p_err = -1;
}

PipedProcess shpipe(char* cmd, int capture) {
    return ppipe((char*[]) { "/bin/sh", "-c", cmd, NULL }, capture);
}

Process shlaunch(char* cmd) {
    return launch((char*[]) { "/bin/sh", "-c", cmd, NULL });
}

void await(Process* proc) {
    // all you need is here: https://linux.die.net/man/2/waitpid
    int status = 0;
    if (waitpid(proc->pid, &status, 0) == -1) {
        fprintf(stderr, "waitpiderr\n");
        status = 1000; // TODO: raise an error here
    }
    proc->exited = WIFEXITED(status);
    proc->code = WEXITSTATUS(status);
}

Process awaitAny() {
    int status = 0;
    pid_t pid = wait(&status);
    if (!pid) fprintf(stderr, "waitpiderr\n"); // TODO: raise an error here

    return (Process) { //
        .pid = pid, //
        .exited = WIFEXITED(status), //
        .code = WEXITSTATUS(status)
    };
}

void awaitAll() {
    int status = 0;
    while (wait(&status) > 0) continue;
}

void update(Process* proc) {
    int status = 0;
    waitpid(proc->pid, &status, WNOHANG);
    proc->code = WEXITSTATUS(status);
    proc->exited = WIFEXITED(status);
}

void test_posix_spawn(void) {
#define TOT 2000
#define PROCS 4

    // benchmark 2000 launches in 4x parallel (2 cores). WALL clock times:
    // (1) /usr/bin/true   1.2s
    // (2) true            1.6s (+400us/call) vs. (1) -- $PATH lookup
    // (3) sh -c true      2.9s (+400us+1300us/call vs. (1) -- shell launch
    // true returns instantly, so overhead is visible.
    // for launching a typical gcc session it shouldn't matter...

    Process proc[4];
    char* cmd[] = { "true", "nantag.c",
        // "/Users/sushant/Downloads/sqlite-amalgamation-3330000/shell.c "
        // "/Users/sushant/Downloads/sqlite-amalgamation-3330000/sqlite3.c",
        NULL };

    for (int j = 0; j < TOT / PROCS; j++) {
        for (int i = 0; i < PROCS; i++) //
            proc[i] = launch(cmd);

        // here count the actual number of valid pids
        for (int i = 0; i < PROCS; i++) {
            // this will be while (proclist has more)
            Process proc = awaitAny();
            if (proc.exited && proc.code)
                printf("Child exited with status %i\n", proc.code);
            // here launch 1 more
        }
    }
}

int main(void) {
    // test_fork_exec();
    // test_posix_spawn();

    char* cmds[] = { "/usr/bin/gcc", "-o", "outf", "--", "-", NULL };
    // Process p = launch(cmds);
    PipedProcess p = ppipe(cmds, JET_PIPE_WRITE);
    pwrite(p, "int main() {return 58;}", 23);
    close(&p);
    awaitAll();
    return EXIT_SUCCESS;
}

/*
void test_posix_spawn_bkp(void)
{
    pid_t pid;
    char* cmd = "gcc";
    char* argv[] = { cmd, "nantag.c", (char*)0 };
    int status[4];
    // puts("Testing posix_spawn");
    // fflush(NULL);
    for (int j = 0; j < 25; j++)
        for (int i = 0; i < 4; i++) {
            if ((status[i]
                    = posix_spawnp(&pid, cmd, NULL, NULL, argv, environ))) {
                printf("error spawning '%s': %s\n", cmd, strerror(status[i]));
            } else {
                // printf("Child id: %i\n", pid);
                fflush(NULL);
                if (waitpid(pid, &status[i], 0) != -1) {
                    if (status[i])
                        printf("Child exited with status %i\n", status[i]);
                } else {
                    printf("waitpid");
                }
            }
        }
}*/