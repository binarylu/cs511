package cs511;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;



public class Ex4 {
    public static void main(String[] args) {

        final int queueLength = 2;
        
    /* get command line argument */
        if (args.length != 1) {
            System.out.println("argument: filename");
            return;
        }

    /* create shared queue */
        ArrayBlockingQueue<MathProblem> sharedQueue = new ArrayBlockingQueue<MathProblem>(queueLength);

    /* create printer thread */
        Thread printer = new Thread(new Printer(sharedQueue));
        printer.start();
        
    /* get going: create parser thread */
        Thread parser = new Thread(new Parser(sharedQueue, args[0]));
        parser.start();

    /* shut down */
        try {
            parser.join();
            printer.join();
        } catch (InterruptedException ex) {
        }
    }

}


class Parser implements Runnable {

    private String fileName;
    private BlockingQueue<MathProblem> sharedQueue;

    public Parser(BlockingQueue<MathProblem> q, String f) {
        sharedQueue = q;
        fileName = f;
    }

    public void run() {

    /* open input file */
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(fileName));
        } catch (FileNotFoundException ex) {
            System.out.println("Cannot open file " + fileName);
            return;
        }

    /* read & process lines from input file */
        try {
            String line;
            while ((line = reader.readLine()) != null) {
                MathProblem problem = null;

                String[] words = line.split("[ ]+");
                int num1 = Integer.parseInt(words[0]);
                int num2 = Integer.parseInt(words[1]);
                String op = words[2];
                if (op.equals("+"))
                    problem = new Addition(num1, num2);
                else if (op.equals("-"))
                    problem = new Subtraction(num1, num2);
                else
                    problem = new Multiplication(num1, num2);
                sharedQueue.put(problem);
            }
            reader.close();
            sharedQueue.put(new Terminator());
        } catch (Exception ex) {
        }

    }
}


class Printer implements Runnable {

    private BlockingQueue<MathProblem> sharedQueue;

    public Printer(BlockingQueue<MathProblem> q) {
        sharedQueue = q;
    }

    public void run() {
        try {
            while (true) {
                MathProblem prob = sharedQueue.take();
                if (prob instanceof Terminator)
                    return;
                FutureTask<Integer> ft = new FutureTask<Integer>(prob);
                Thread t = new Thread(ft);
                t.start();
                System.out.println(prob.getNum1() + " " + prob.getOp() + " " + prob.getNum2() + " = " + ft.get());
            }
        } catch (Exception ex) {
        }
    }
}


class MathProblem implements Callable<Integer> {

    int num1;
    int num2;
    String op;

    public MathProblem(int x, int y, String op) {
        num1 = x;
        num2 = y;
        this.op = op;
    }

    public Integer getNum1() {
        return num1;
    }

    public Integer getNum2() {
        return num2;
    }

    public String getOp() {
        return op;
    }

    /* call() will be overridden by subclasses */
    public Integer call() {
        return 0;
    }

}


class Addition extends MathProblem {

    public Addition(int x, int y) {
        super(x, y, "+");
    }
    
    public Integer call() {
        return new Integer(num1 + num2);
    }
}


class Subtraction extends MathProblem {

    public Subtraction(int x, int y) {
        super(x, y, "-");
    }
    
    public Integer call() {
        return new Integer(num1 - num2);
    }
}


class Multiplication extends MathProblem {

    public Multiplication(int x, int y) {
        super(x, y, "*");
    }
    
    public Integer call() {
        return new Integer(num1 * num2);
    }
}


class Terminator extends MathProblem {

    public Terminator() {
        super(0, 0, null);
    }

}
