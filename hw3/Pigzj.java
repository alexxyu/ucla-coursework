import java.io.*;

// TODO: Exception and error handling
// TODO: Improve efficiency (e.g. don't copy arrays)

public class Pigzj {

    private static int availableProcessors = Runtime.getRuntime().availableProcessors();

    public static void main(String[] args) throws IOException {
        if(args.length != 0 && args.length != 2) {
            System.err.println("Unrecognized arguments");
            return;
        }

        int numProcessors = availableProcessors;
        if(args.length == 2) {
            if(args[0].equals("-p")) {
                try {
                    numProcessors = Integer.parseInt(args[1]);
                    if(numProcessors < 0 || numProcessors > availableProcessors) {
                        System.err.println("Invalid number of processes");
                        return;
                    }
                } catch(NumberFormatException e) {
                    System.err.println("Invalid number of processes");
                    return;
                }
            } else {
                System.err.println("Unrecognized option");
                return;
            }
        }
        
        InputStream in = System.in;
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        PigzjOutputStream pigzjOut = new PigzjOutputStream(in, out, numProcessors);

        pigzjOut.compress();
        pigzjOut.finish();

        in.close();
        out.close();
    }

}
