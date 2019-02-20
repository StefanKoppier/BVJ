class Main {
    public static void main() {
        int[] array = { 1, 2, 3 };
        int m;

        for (int i = 0; i < 3; i++)
            m = max(m, array[i]);

        assert m == 3 : "m != 3";
    }

    public static int max(int x, int y) {
        return x > y ? x : y;
    }
}