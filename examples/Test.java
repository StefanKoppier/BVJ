class Main {
    public static void main() {
        int x = 0;
        if (true) {
            x = 1;
        } else {
            x = Test.getId();
        }
        assert x == 1;
    }
}
