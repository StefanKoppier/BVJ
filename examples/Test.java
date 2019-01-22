class Main {
    public static void main(String[] args) {
        while (true) {
            assert 1 == 1;
            while (false) {
                assert 2 == 2;
            }
            assert 3 == 3;
        }
        ;
    }
}
