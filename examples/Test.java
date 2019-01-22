class Main {
    public static void main(String[] args) {
        a: while (true) {
            while (true) {
                assert true;
                continue a;
                assert true;
            }
        }
    }
}
