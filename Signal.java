package data;
import exceptions.UnpresentSignalException;

public class Signal<Type> extends Event{
	private Type t;
	private boolean loop_compute = false;

	public Signal(){
		present = false;
	}

	public Signal(Type value){
		t = value;
		present = true;
	}

	public Signal(Type value, boolean isPresent){
		t = value;
		present = isPresent;
	}

	public void setT(Type value){
		t = value;
		present = true;
	}

	public void setT(Signal<Type> s){
		present = s.present;
		if(present)
			t = s.t;
	}

	public void unsetT(){
		present = false;
	}

	public Type getT() throws UnpresentSignalException{
		if(present)
			return t;
		else
			throw new UnpresentSignalException();
	}
	
	public Type t(){
		return t;
	}

	public void new_loop(){
		loop_compute = false;
	}

	public void now_computed(){
		loop_compute = true;
	}

	public boolean isNotComputed(){
		return !loop_compute;
	}
}
