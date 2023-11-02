
package Modelo;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;

public interface Wrapper3270 {
    boolean conectar(String ipMainframe, PrintWriter out, InputStream in)throws  IOException, InterruptedException;
    void login(PrintWriter out, InputStream in, String usuario, String contrasenia, String programa)throws IOException, InterruptedException;

    void escribir(String msg, PrintWriter out);
    String leer(InputStream in) throws IOException, InterruptedException;
    String obtenerPantalla(String texto, PrintWriter out, InputStream in) throws IOException, InterruptedException;
    String esperaOk(InputStream in) throws IOException, InterruptedException;
}
