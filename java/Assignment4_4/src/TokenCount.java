import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.Map.Entry;

public class TokenCount {
    public static void main(String[] args) throws Exception {
    if (args.length != 2) {
        System.out.println("usage: java TokenCount number-of-pages XML-file");
        System.exit(0);
    }
    System.out.println("Assignment4_4");
    Integer numPages = Integer.parseInt(args[0]);
    
    final int queueLength = 100;

    // print number of available processors
    System.out.println(Runtime.getRuntime().availableProcessors() + " available processors");
    int numProcessors = Runtime.getRuntime().availableProcessors();

    ArrayBlockingQueue<Page> sharedQueue = new ArrayBlockingQueue<Page>(queueLength);
    ConcurrentHashMap<String, Integer> tokenFreq = new ConcurrentHashMap<String, Integer>();
    ExecutorService pool = Executors.newCachedThreadPool();
    
    ArrayList<Future<ConcurrentHashMap<String, Integer>>> arr = new ArrayList<Future<ConcurrentHashMap<String, Integer>>>();

/* begin timed code ... */
    final long before = System.nanoTime();
    for (int i = 0; i < numProcessors - 1; ++i) {
    	arr.add(pool.submit(new consumer(sharedQueue)));
    }
    Iterable<Page> allPages = new Pages(numPages, args[1]);
    try {
    	for (Page pg: allPages)
    		sharedQueue.put(pg);
        for (int i = 0; i < numProcessors - 1; ++i)
        	sharedQueue.put(new PoisonPill());
    } catch (InterruptedException e) {
		e.printStackTrace();
	}
    pool.shutdown();
    pool.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS);
    for (int i = 0; i < numProcessors - 1; ++i) {
    	ConcurrentHashMap<String, Integer> freq = arr.get(i).get();
    	Iterator<Entry<String, Integer>> iter = freq.entrySet().iterator();
    	while (iter.hasNext()) {
    		ConcurrentHashMap.Entry<String, Integer> entry = iter.next();
    		String s = entry.getKey();
    		Integer count = entry.getValue();
    		
			Integer currentCount = tokenFreq.get(s);
			if (currentCount == null)
				tokenFreq.put(s, count);
			else
				tokenFreq.put(s, currentCount + count);
    	}
    }
    final long after = System.nanoTime();
/* ... end  timed code */

    System.out.println("Time to process " + numPages + " pages = " + (after - before)/1000000 + " milliseconds");

    // sort tokenFreq by value & print top 30 most common tokens
    Set<Entry<String, Integer>> entries = tokenFreq.entrySet();
        ArrayList<Entry<String, Integer>> list = new ArrayList<Entry<String, Integer>>(entries);
        Collections.sort(list, new Comparator<Map.Entry<String, Integer>>()
              {
                  public int compare(Map.Entry<String, Integer> obj1, Map.Entry<String, Integer> obj2)
                  {
                  return (obj2.getValue()).compareTo(obj1.getValue());
                  }
              } );
        System.out.println(list.size());
        for(int i=0; i<30; i++)
            System.out.println(list.get(i).getKey() + " appears " + list.get(i).getValue() + " times");
    }
}

class consumer implements Callable<ConcurrentHashMap<String, Integer>> {
	private BlockingQueue<Page> sharedQueue;
	private ConcurrentHashMap<String, Integer> tokenFreq = new ConcurrentHashMap<String, Integer>();
	
	public consumer(BlockingQueue<Page> q) {
		sharedQueue = q;
	}

	@Override
	public ConcurrentHashMap<String, Integer> call() throws Exception {
		try {
			while (true) {
				Page pg = sharedQueue.take();
				if (pg instanceof PoisonPill)
					return tokenFreq;
				Iterable<String> allTokens = new Words(pg.getText());
				for (String s : allTokens) {
					Integer currentCount = tokenFreq.get(s);
					if (currentCount == null)
						tokenFreq.put(s, 1);
					else
						tokenFreq.put(s, currentCount + 1);
				}
				return tokenFreq;
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		return null;
	}
}