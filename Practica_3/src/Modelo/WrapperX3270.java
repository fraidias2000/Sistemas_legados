
package Modelo;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;

public class WrapperX3270 implements Wrapper3270{
    
    private final String ASCII = "ascii";
    private final String ENTER = "ENTER";
    private final String OK = "ok";
    private final String OBTENER_PANTALLA_PRESS = "Press";
    private final String OBTENER_PANTALLA_MUSIC_USERID = "MUSIC Userid:";
    private final String OBTENER_PANTALLA_MUSIC = "MUSIC";
    private final String OBTENER_PANTALLA_TASK2 = "tasks2.job";
    
     public void escribir(String msg, PrintWriter out) {
        out.println(msg);
        out.flush();
    }

    public String leer(InputStream in) throws IOException, InterruptedException {
        StringBuilder status = new StringBuilder();
        while (in.available() == 0);
        while (in.available() > 0) {
            status.append((char) in.read());
        }
        return status.toString();
    }

    public String obtenerPantalla(String texto, PrintWriter out, InputStream in) throws IOException, 
            InterruptedException {
        String salida;
        do {
            escribir(ASCII, out);
            salida = leer(in);
        } while (salida.indexOf(texto) == -1);
        return salida;
    }
    
    public String esperaOk(InputStream in) throws IOException, InterruptedException {
        String salida;
        do {
            salida = leer(in);
        } while (salida.indexOf(OK) == -1);
        return salida;
    }
    
    @Override
    public boolean conectar(String ipMainframe, PrintWriter out, InputStream in)throws IOException, InterruptedException{
        escribir(ipMainframe, out);
        esperaOk(in);
        obtenerPantalla(OBTENER_PANTALLA_PRESS, out, in);
        escribir(ENTER, out);
        esperaOk(in);
        obtenerPantalla(OBTENER_PANTALLA_MUSIC, out, in);  
        return true;
    }
    
    public void login(PrintWriter out, InputStream in, String usuario, String contrasenia, String programa)throws IOException, InterruptedException{
        escribir(usuario,out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(contrasenia, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        obtenerPantalla(ENTER, out, in);
        escribir(ENTER, out);
        esperaOk(in);
        obtenerPantalla(OBTENER_PANTALLA_MUSIC.toUpperCase(), out, in);
        escribir(programa, out);
        esperaOk(in);
        obtenerPantalla(OBTENER_PANTALLA_TASK2, out, in);
        escribir(ENTER, out);
        esperaOk(in);
    }
}
