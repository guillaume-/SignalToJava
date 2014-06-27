package data;

public class Signal<Type> extends Event{
	private Type t;
	
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
}
