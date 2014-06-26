package data;

public class DataStruc {
	private int a0 = 1, b0 = 1, c0 = 0, d0 = 0; /* pour accéder à la valeur des variables (...à t-0) */
	private final int constant1 = 1;
	
	public DataStruc(int a, int b){
		a0 = a;
		b0 = b;
	}
	
	public void body(int n){
		synchronized(this){
			uniq(n);
			notifyAll();
			try{
				wait();
			} catch (InterruptedException e){
				e.printStackTrace();
			}
		}
	}
	
	public void uniq(int num){
		switch(num){
			case 0 :
				c0 = a0;
				System.out.printf("c0 = %d;\n", c0);
				break;
			case 1 :
				d0 = b0 + constant1;
				System.out.printf("d0 = %d;\n", d0);
		}
	}
}
