import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Map.Entry;

public class TokenCount {
    private static final ConcurrentHashMap<String, AtomicInteger> tokenFreq = new ConcurrentHashMap<String, AtomicInteger>();

    public static void main(String[] args) throws Exception {
    if (args.length != 2) {
        System.out.println("usage: java TokenCount number-of-pages XML-file");
        System.exit(0);
    }
    System.out.println("Assignment4_3");
    Integer numPages = Integer.parseInt(args[0]);
    
    final int queueLength = 100;

    // print number of available processors
    System.out.println(Runtime.getRuntime().availableProcessors() + " available processors");
    int numProcessors = Runtime.getRuntime().availableProcessors();

    ArrayBlockingQueue<Page> sharedQueue = new ArrayBlockingQueue<Page>(queueLength);
    Thread th_producer = new Thread(new producer(sharedQueue, numPages, args[1]));
    ExecutorService pool = Executors.newCachedThreadPool();

/* begin timed code ... */
    final long before = System.nanoTime();
    th_producer.start();
    for (int i = 0; i < numProcessors - 1; ++i) {
        pool.execute(new consumer(sharedQueue, tokenFreq));
    }
    th_producer.join();
    pool.shutdown();
    pool.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS);
    final long after = System.nanoTime();
/* ... end  timed code */

    System.out.println("Time to process " + numPages + " pages = " + (after - before)/1000000 + " milliseconds");

    // sort tokenFreq by value & print top 30 most common tokens
    Set<Entry<String, AtomicInteger>> entries = tokenFreq.entrySet();
        ArrayList<Entry<String, AtomicInteger>> list = new ArrayList<Entry<String, AtomicInteger>>(entries);
        Collections.sort(list, new Comparator<Map.Entry<String, AtomicInteger>>()
              {
                  public int compare(Map.Entry<String, AtomicInteger> obj1, Map.Entry<String, AtomicInteger> obj2)
                  {
                  return (new Integer(obj2.getValue().get())).compareTo(new Integer(obj1.getValue().get()));
                  }
              } );
        System.out.println(list.size());
        for(int i=0; i<30; i++)
            System.out.println(list.get(i).getKey() + " appears " + list.get(i).getValue() + " times");
    }
}

class producer implements Runnable {
    private BlockingQueue<Page> sharedQueue;
    private String fileName;
    private Integer numPages;
    
    public producer(BlockingQueue<Page> q, Integer num, String f) {
        sharedQueue = q;
        numPages = num;
        fileName = f;
	}

	public void run() {
        Iterable<Page> allPages = new Pages(numPages, fileName);
        int numProcessors = Runtime.getRuntime().availableProcessors();
        try {
        	for (Page pg: allPages)
        		sharedQueue.put(pg);
            for (int i = 0; i < numProcessors - 1; ++i)
            	sharedQueue.put(new PoisonPill());
        } catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}

class consumer implements Runnable {
	private BlockingQueue<Page> sharedQueue;
	private ConcurrentHashMap<String, AtomicInteger> tokenFreq;
	
	public consumer(BlockingQueue<Page> q, ConcurrentHashMap<String, AtomicInteger> map) {
		sharedQueue = q;
		tokenFreq = map;
	}
	
	public void run() {
		try {
			while (true) {
				Page pg = sharedQueue.take();
				if (pg instanceof PoisonPill)
					break;
				Iterable<String> allTokens = new Words(pg.getText());
				for (String s : allTokens) {
					tokenFreq.putIfAbsent(s, new AtomicInteger(0));
					tokenFreq.get(s).incrementAndGet();
				}
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}