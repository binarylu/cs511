package test;

public class My_Class {
	public static void main(String argv[]) throws InterruptedException {
		MyThread mt = new MyThread("haha");
		Thread t1 = new Thread(mt);
		//Thread t2 = new Thread(mt);
		//Thread t3 = new Thread(mt);
		t1.setDaemon(true);
		t1.start();
		//t2.start();
		//t3.start();

		//System.out.println("t1: " + t1.getState());
		//System.out.println("t2: " + t2.getState());
		//System.out.println("t3: " + t3.getState());
		/*t1.join();
		t2.join();
		t3.join();*/
		
		t1.interrupt();
		t1.join();
		System.out.println(t1.getState());
		System.out.println(t1);
	}
}

class MyThread implements Runnable
{
	int index = 0;
	String my_name;
	
	public MyThread(String name) {
		my_name = name;
	}

	@Override
	public void run() {
		// TODO Auto-generated method stub
		while(true) {
			System.out.println(Thread.currentThread().getName() + "(" + my_name + "):" + index++);
			if (index == 4) break;
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
}