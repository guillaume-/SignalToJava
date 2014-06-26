package thread;

import data.DataStruc;

public class SigThread extends Thread {
	private DataStruc d;
	private int n;

	public SigThread(DataStruc ds, int num){
		d = ds;
		n = num;
	}
	
	@Override
	public void run(){
		for(;;)
			d.body(n);
	}
}
