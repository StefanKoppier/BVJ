class Main {
    public static void main(String[] args) {
        int[] x = { 1, 2, 3 };

        for (int i = 0; i < 3; i++) {
            assert x[i] == i - 1 : "x[i] == i - 1";
        }
    }
}
