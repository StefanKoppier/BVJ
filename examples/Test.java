class Main {
    public static void main() {
        int[][][] array = new int[2][1][1];

        array[0][0][0] = 2;
        array[1][0][0] = 1;

        assert array[0][0][0] == 2;
        assert array[1][0][0] == 1;
    }
}
