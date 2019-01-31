class Main {
    public static void main(String[] args) {
        b: while (true) {
            a: for (;true;) {
                int x = 0;
                continue b;
                int y = 0;
            }
        }
        assert 1 == 1;
    }
}
