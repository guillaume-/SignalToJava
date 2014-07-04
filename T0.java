package thread;
import data.GlobalData;
import data.Signal;
import exceptions.UnpresentSignalException;

public class T0 extends thread{
	Signal<Integer> a0 = new Signal<Integer>(7), 
					w0 = new Signal<>(),
					x0 = new Signal<>(),
					y0 = new Signal<>();

	public T0(int n, GlobalData gd) {
		super(n, gd);
	}

	@Override
	public void reset(){
		super.reset();
		w0.new_loop();
		x0.new_loop();
		y0.new_loop();
	}

	@Override
	public void uniq(){
		try{
			w0.setT(a0.getT() + data.b0.getT());
		}catch(UnpresentSignalException e){
			w0.unsetT();
		}
		try{
			x0.setT(a0.getT() * w0.getT());
		}catch(UnpresentSignalException e){
			x0.unsetT();
		}
		try{
			y0.setT((data.b0.getT()*x0.getT())-w0.getT());
		}catch(UnpresentSignalException e){
			y0.unsetT();
		}
		synchronized(data.u0){
			try{
				while(data.u0.isNotComputed())
					data.u0.wait();
			}catch(InterruptedException e){}
			try{
				data.z0.setT(x0.getT() + y0.getT() + data.u0.getT());
			}catch(UnpresentSignalException e){
				data.z0.unsetT();
			}
		}
	}
}
