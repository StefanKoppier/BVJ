class Main {
    public static void main() {
        Integer value = new Integer(0);

        for (int i = 0; i < 3; i++) {
            value.setInt(i);
            assert value.getInt() == i;
        }
    }
}

class Integer {
    private int value;

    public Integer(int value) {
        this.value = value;
    }

    public int getInt() {
        return value;
    }

    public void setInt(int value) {
        this.value = value;
    }
}