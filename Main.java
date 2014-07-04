package main;

import thread.T0;
import thread.T1;
import thread.thread;
import data.GlobalData;

public class Main{
	public static void main(String[]args){
		GlobalData data = new GlobalData();
		thread t0 = new T0(0, data), t1 = new T1(1, data);
		data.add_thread(t0);
		data.add_thread(t1);
		t0.start();
		t1.start();
	}
}
