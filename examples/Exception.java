class Main {
    public static void main(String[] argv) {
        int[] array = { 1 };

        try {
            throw new Exception();
        } catch (Exception e) {
            try {
                assert e != null;
            } catch (Exception e) {

            } finally {
                ;
            }
            ;
        } finally {
            assert true;
        }
    }
}
