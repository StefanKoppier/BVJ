class Main {
    public static void main() {
        assert (square(2) + square(2)) == 8 : "2^2 + 2^2 != 8";
    }

    public int square(int x) {
        return x * x;
    }
}