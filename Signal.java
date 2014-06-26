package data;

public class Signal<Type> extends Event{
	public Type t;
	
	public Signal(Type value){
		t = value;
		present = true;
	}
	
	public void setT(Type value){
		t = value;
		present = true;
	}
	
	public void unsetT(){
		present = false;
	}
}
