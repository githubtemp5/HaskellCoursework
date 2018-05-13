/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package haskellfilereader;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;

/**
 *
 * @author up826133
 */
public class HaskellFileReader {

 public void read() throws FileNotFoundException{
     String fileName = "N://1MATHFUN/Haskell/Coursework/filmsCopy.txt";
  try {
    File file = new File(fileName);
    BufferedReader reader = new BufferedReader(new FileReader(file));

    String line;
    int counter=0;
    ArrayList <String> tempLine = new ArrayList<String>();
    while ((line = reader.readLine()) != null) {
        tempLine.add(line);
        counter++;
        if(counter % 6==0){
            System.out.println("("+tempLine.get(0)+", "+tempLine.get(1)+", "+tempLine.get(2)+", ["+tempLine.get(3)+"], ["+tempLine.get(4)+"]), ");
        tempLine = new ArrayList<String>();
        }
     //   System.out.println(line);
        
    }
      System.out.println(counter);
  }catch(Exception e){}
     
     
 }
    public static void main(String[] args) {
HaskellFileReader h = new HaskellFileReader();
try{
h.read();
}catch(Exception e){
}
    }
    
}
