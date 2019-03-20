class Main {
    public static void main() {
        int[] array = { 1 };

        try {
            throw new Exception();
        } catch (Exception e) {
            assert e != null;
        } finally {
            assert true;
        }
    }
}

class Exception {
    public Exception() {

    }
}