class Main {
    public static void main(String[] argv) {
        Integer value = new Integer(0);

        value.setInt(1);
        assert value.getInt() == 1 : "value.getInt() != 1";
    }
}

class Integer {
    private int value;

    public Integer(int value) {
        this.value = value;
    }

    public int getInt() {
        return this.value;
    }

    public void setInt(int value) {
        this.value = value;
    }
}
