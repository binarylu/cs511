package cs511;

import java.util.concurrent.*;

public class test {
	public static void main(String[] argv) throws InterruptedException, ExecutionException {
//		CallableExample c = new CallableExample();
//		FutureTask<String> ft = new FutureTask<String>(c);
//		Thread t = new Thread(ft);
//		t.start();
//		t.join();
//		String s = ft.get();
//		System.out.println(s);
		int numThreads = 5;
		ExecutorService pool = Executors.newFixedThreadPool(numThreads);
		
		CallableExample c = new CallableExample();
		Future<String> f = pool.submit(c);
		String s = f.get();
		System.out.println(s);
	}
}

class CallableExample implements Callable<String> {

	@Override
	public String call() throws Exception {
		return "I am call()";
	}
	
}