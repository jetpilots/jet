
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <math.h>
#include <sys/time.h>
// int main() { }

unsigned long counter;
int MAX;
int ALARMcount;
int SECOND;

static unsigned long long M, Mcount[4];

double Mpow(double a, double b) {
    M = 0; //_Mpow++;
    return pow(a, b);
}
double Msqrt(double a) {
    M = 1; //_Msqrt++;
    return sqrt(a);
}
double Mlog(double a) {
    M = 2; //_Mlog++;
    return log(a);
}
void ALARMhandler(int sig) {
    // signal(SIGPROF, SIG_IGN);
    ALARMcount++;
    // printf("\n*** ALARMhandler --> alarm received no. %d.\n", ALARMcount);
    // printf("*** ALARMhandler --> counter = %ld\n", counter);
    // printf("*** ALARMhandler --> alarm reset to %d seconds\n", SECOND);
    // unsigned long long tot = Mcount[0] + Mcount[1] + Mcount[2];
    // printf("%g %g %g\n", 1.0 * Mcount[0] / tot, 1.0 * Mcount[1] / tot,
    //     1.0 * Mcount[2] / tot);
    if (ALARMcount == MAX) {
        unsigned long long tot = Mcount[0] + Mcount[1] + Mcount[2] + Mcount[3];
        printf("%.2f%% %.2f%% %.2f%% %.2f%%\n", 100.0 * Mcount[0] / tot,
            100.0 * Mcount[1] / tot, 100.0 * Mcount[2] / tot,
            100.0 * Mcount[3] / tot);
        // printf("*** ALARMhandler --> Maximum alarm count reached.  exit\n");
        exit(0);
    }
    Mcount[M]++;
    counter = 0; /* otherwise, reset counter */
    // signal(SIGPROF, ALARMhandler); /* reinstall the handler    */
    alarm(SECOND); /* set alarm for next run   */
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        printf("Use: %s seconds max-alarm-count\n", argv[0]);
        printf("No. of seconds is set to 1\n");
        SECOND = 1;
        printf("Max number of alarms set to 5\n");
        MAX = 5;
    } else {
        SECOND = atoi(argv[1]);
        MAX = atoi(argv[2]);
    }
    counter = 0;
    ALARMcount = 0;
    signal(SIGPROF, ALARMhandler);
    // printf("Alarm set to %d seconds and is ticking now.....\n", SECOND);

    struct itimerval it
        = { .it_interval.tv_usec = 10000, .it_value.tv_usec = 10000 };
    setitimer(ITIMER_PROF, &it, NULL);
    // alarm(SECOND);
    unsigned long long i;
    while (i < 10000000000) {
        M = 3;
        i++;
        int x = Mpow(1.9 * i, 1.9) + Mlog(i * 6.5) + Msqrt(i);
    }
    // while (1) printf("%d\r", counter++);
}