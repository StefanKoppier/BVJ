class Main {
    public static void main() {
        int x = square(square(1));
    }

    public static int square(int x) {
        return x * x;
    }
}