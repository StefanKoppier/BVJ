class Main {
    public static void main(String[] argv) {
        Integer value = new Integer(0);

        value.setInt(1);
        assert value.getInt() == 1 : "value.getInt() != 1";
    }
}

class Integer {
    private int x;

    public Integer(int value) {
        x = value;
    }

    public int getInt() {
        return this.x;
    }

    public void setInt(int value) {
        this.x = value;
    }
}
