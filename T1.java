package thread;

import data.GlobalData;
import exceptions.UnpresentSignalException;

public class T1 extends thread{

	public T1(int n, GlobalData gd) {
		super(n, gd);
	}

	@Override
	public void uniq(){
		synchronized(data.u0){
			try{
				data.u0.setT(2*data.b0.getT());
			}catch(UnpresentSignalException e){
				data.u0.unsetT();
			}
			data.u0.now_computed();
			data.u0.notify();
		}
		try{
			data.v0.setT(data.u0.getT()-1);
		}catch(UnpresentSignalException e){
			data.v0.unsetT();
		}
	}
}
