package data;

public class DataStruc {
	private Signal<Integer> a0 = new Signal<Integer>(1), b0 = new Signal<Integer>(1); /* pour accéder à la valeur des variables (...à t-0) */
	private Signal<Integer> c0 = new Signal<Integer>(), d0 = new Signal<Integer>();
	private Signal<Integer> loc0 = new Signal<Integer>();
	private final Signal<Integer> constant1 = new Signal<Integer>(1);

	public DataStruc(int a, int b){
		a0.setT(a);
		b0.setT(b);
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
				c0.setT(a0);
				try{
					System.out.printf("c0 = %d;\n", c0.getT());
				}catch(UnpresentSignalException e){
					e.printStackTrace();
				}
				break;
			case 1 :
				try{
					d0.setT(b0.getT() + constant1.getT());
				}catch(UnpresentSignalException e){
					d0.unsetT();
				}
				try{
					System.out.printf("d0 = %d;\n", d0.getT());
				}catch(UnpresentSignalException e){
					System.out.printf("d0 not present\n");
				}
				break;
			case 2 :
				loc0.setT(5);
		}
	}
}
