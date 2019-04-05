class Main {
    public static void main(String[] argv) {
        int[] array = { 0, 1, 2 };

        for (int i = 0; true; i++) {
            assert array[i] == i : "array[i] != i";
            {
                break;
            }
        }
    }
}
