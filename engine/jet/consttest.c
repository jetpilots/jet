struct Arr {
  int* ref;
  int c;
};
int main() {
  const struct Arr a = { (int[]) { 8, 9, 7, 6 }, 4 };
  a.ref[9] = 8;
}