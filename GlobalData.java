package data;

import java.util.ArrayList;
import java.util.List;

import thread.thread;

public class GlobalData {
	public Signal<Integer>	b0 = new Signal<Integer>(8),
							u0 = new Signal<Integer>(),
							v0 = new Signal<Integer>(),
							z0 = new Signal<Integer>();
	public List<thread> threads = new ArrayList<thread>();
	public boolean end_loop = false;

	public void add_thread(thread t){
		threads.add(t);
	}

	public boolean allThreadLoopEnded(){
		for(int i=0; i<Constantes.NB_THREADS; i++)
			if(!threads.get(i).has_looped)
				return false;
		return true;
	}

	public boolean anyThreadLoopEnded(){
		for(int i=0; i<Constantes.NB_THREADS; i++)
			if(threads.get(i).has_looped)
				return false;
		return true;
	}

	public void reset(){
		u0.new_loop(); /* u synchronisé */
	}
}
