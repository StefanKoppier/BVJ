class Main {
    public static void main(String[] argv) {
        int[] array = { 0, 1, 2 };

        for (int i = 0; i < array.length; i++) {
            assert array[i] == i : "array[i] != i";
        }
    }

    public static int id(int x) {
        if (x == 0) {
            return x;
        } else {
            return x;
        }
    }
}