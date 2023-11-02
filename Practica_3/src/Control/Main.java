
package Control;

import Modelo.WrapperX3270Music;
import Vista.WrapperVista;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Main {


    public static void main(String[] args) {
        
        try {
            WrapperX3270Music wrapper = new WrapperX3270Music();            
            WrapperVista wrapperVista = new WrapperVista(wrapper);            
            wrapper.nuevoObservador(wrapperVista);     
        } catch (IOException ex) {
            System.out.println(ex);
            Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
        }
        
    }

}
