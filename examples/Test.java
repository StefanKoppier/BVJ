class Main {
    public static void main() {
        int x = square(2) + id(1);
        assert x == 5 : "x == 4";
    }

    public static int square(int x) {
        return x * x;
    }

    public static int id(int x) {
        return x;
    }
}