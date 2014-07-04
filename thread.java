package thread;
import data.GlobalData;

public abstract class thread extends Thread{
	private int num;
	protected GlobalData data;
	public boolean has_looped;

	public thread(int n, GlobalData gd){
		data = gd;
		has_looped = false;
		num = n;
	}

	@Override
	public void run(){
		for(;;)
			body();
	}

	public void body(){
		System.out.println("Début du thread n°"+num);
		uniq();
		System.out.println("Fin du thread n°"+num);
		synchronized(data){
			data.threads.get(num).has_looped = true;
			data.notifyAll();
			try{
				while((!data.allThreadLoopEnded()) && !data.end_loop)
					data.wait();
			}catch(InterruptedException e){}
			reset();
			data.end_loop = true;
			data.notifyAll();
			data.threads.get(num).has_looped = false;
			data.notifyAll();
			try{
				while((!data.anyThreadLoopEnded()) && data.end_loop)
					data.wait();
			}catch(InterruptedException e){}
			data.end_loop = false;
			data.notifyAll();
		}
	}

	public void reset(){
		data.reset();
	}

	public abstract void uniq();

	public int id(){
		return num;
	}
}
