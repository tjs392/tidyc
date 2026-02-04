struct Point {
    int x;
    int y;
};

int add(int a, int b) {
    return a + b;
}

int main(void) {
    int x = 5;
    int y = 10;
    int sum = add(x, y);
    return sum;
}