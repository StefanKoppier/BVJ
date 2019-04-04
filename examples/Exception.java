class Main {
    public static void main(String[] argv) {
        int[] array = { 1 };

        try {
            throw new Exception();
        } catch (Exception e) {
            assert e != null;
        } finally {
            assert true;
        }
        ;
    }
}
