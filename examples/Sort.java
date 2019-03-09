class Main {
    public static void main() {
        int[] elems = new int[3];
        elems[0] = 2;
        elems[1] = 1;
        elems[2] = 3;
        sort(elems, 0, elems.length - 1);
        assert (elems[0] == 1) && (elems[1] == 2) && (elems[2] == 3);
    }

    public static int partition(int[] array, int low, int high)
    {
        int pivot = array[high];
        int i = low - 1;
        int temp;

        for (int j = low; j < high; j++) {
            if (array[j] <= pivot)
            {
                i++;
                temp = array[i];
                array[i] = array[j];
                array[j] = temp;
            }
        }

        temp = array[i+1];
        array[i+1] = array[high];
        array[high] = temp;

        return i + 1;
    }

    public static void sort(int[] array, int low, int high) {
        if (low < high) {
            int pi = partition(array, low, high);
            sort(array, low, pi - 1);
            sort(array, pi + 1, high);
        }
    }
}