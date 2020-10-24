
const char* foo = "The quick brown fox";

typedef enum TestEnum { TE1 = foo } TestEnum;

int main() {
    TestEnum e = TE1;
    printf("%s\n", e);
}