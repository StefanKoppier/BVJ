class Main {
    private static int counter = 0;

    public static void main(String[] args) {
        for (Int i = 0; i < args.length; ++i) {
            counter++;
        }
    }
}

/*

struct class_Main
{
} class_Main_t

int static_variable_counter = 0;

void static_void_main(const char* args, int args_length)
{
    for (int i = 0; i < args_length; ++i)
    {
        static_variable_counter++;
    }
}

*/