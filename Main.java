package main;

import java.util.ArrayList;
import java.util.List;

import thread.SigThread;
import data.DataStruc;
import usable.Usable;

public class Example2 {
	public static void main(String[] args){
		DataStruc d = new DataStruc(1, 1);
		List<SigThread> l = new ArrayList<SigThread>();
		for(int i=0; i<2; i++)
			l.add(new SigThread(d, i));
		for(SigThread t : l)
			t.start();
	}
}
