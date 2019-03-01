class Main {
    public static void main() {
        int a = id(1) + id(2);
        assert a == 3;
    }

    public static int id2(int x) {
        return x;
    }

    public static int id(int x) {
        return id2(x) + id2(0);
    }
}