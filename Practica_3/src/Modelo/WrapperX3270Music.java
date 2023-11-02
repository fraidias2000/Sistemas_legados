
package Modelo;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Scanner;

public class WrapperX3270Music extends WrapperX3270{
//    private final String S3270PATH = "D:\\Escritorio\\Universidad\\4\\Primer_cuatrimestre\\Sistemas_legados\\Practicas\\Practica3\\wc3270\\s3270.exe";
    private final String S3270PATH = ".\\dist\\s3270.exe";
    private final String IP_MAINFRAME = "connect 155.210.71.101:423";
    private final String PROGRAMA_MAINFRAME = "string(\"tasks2.job\")" ;
    private final String CONFIRM = "string(\"Y\")";
    private final String NEGAR = "string(\"N\")";
    private final int CONFIRMAR_DECISION = 1;
    private final int NO_CONFIRMAR_DECISION = 2;
    private final String ENTER = "ENTER";
    private final String INICIO_MENSAJE_MAINFRAME = "string(\" ";
    private final String FINAL_MENSAJE_MAINFRAME = " \")";
    private final String DESCONECTADO = "disconnect";
    private final String OFF = "string(\"off\")";
    private final String COMMAND = "Command";
    private final String BYE = "BYE";
    private final String QUIT = "quit";
    private final String MENSAJE_SALIR_APLICACION = "Gracias por utilizar nuestra aplicacion";
    private final String OBTENER_PANTALLA_PRESS = "PRESS";
    private final String OBTENER_PANTALLA_NEW = "NEW";
    private final String OBTENER_PANTALLA_LIST = "LIST";
    private final String OBTENER_PANTALLA_CONFIRM = "CONFIRM";
    private final String OBTENER_PANTALLA_REMOVE = "REMOVE";
    //MENU MAINFRAME
    private final String NEW_TASK_FILE = "N";
    private final String ADD_TASK = "A";
    private final String REMOVE_TASK = "R";
    private final String SEARCH_TASK = "T";
    private final String LIST_TASK = "L";
    private final String SAVE_TASK = "S";
    private final String SALIR_APLICACION = "E"; 
    private final String TAREA_ANYADIDA_CORRECTAMENTE = "La tarea ha sido añadida correctamente, ya puede cerrar esta ventana"; 
    private final String ELEGIR_NEW_TASK_FILE = "string(\"N\")";
    private final String ELEGIR_ADD_TASK = "string(\"A\")";
    private final String ELEGIR_REMOVE_TASK = "string(\"R\")";
    private final String ELEGIR_SEARCH_TASK = "string(\"T\")";
    private final String ELEGIR_LIST_TASK = "string(\"L\")";
    private final String ELEGIR_SAVE_TASK = "string(\"S\")";
    private final String ELEGIR_SALIR_APLICACION = "string(\"E\")";
    private static InputStream in;
    private static PrintWriter out;
    Scanner sc;
    private int pasoRemoveTask = 0;
    private static Process s3270;
    private PropertyChangeSupport observadores;
    
    public WrapperX3270Music() throws IOException {
        s3270 = Runtime.getRuntime().exec(S3270PATH);
        in = s3270.getInputStream();
        out = new PrintWriter(new OutputStreamWriter(s3270.getOutputStream()));
        observadores = new PropertyChangeSupport(this);
        sc = new Scanner(System.in);
    }

    public boolean conectarMainframe()
            throws IOException, InterruptedException {   
        return super.conectar(IP_MAINFRAME, out, in);         
    }
    
    public void login(String userId, String password) throws IOException, InterruptedException{
       super.login(out, in, userId, password, PROGRAMA_MAINFRAME);

    }
    
    public String NewTaskFile(int decision)throws IOException, InterruptedException{
       String mensaje = null;
       switch (decision){
            case 0:
                escribir(ELEGIR_NEW_TASK_FILE, out);
                esperaOk(in);
                escribir(ENTER, out);
                esperaOk(in);
                mensaje = obtenerPantalla(OBTENER_PANTALLA_NEW, out, in);
            break;
            case CONFIRMAR_DECISION:
                escribir(CONFIRM, out);
                esperaOk(in);
                escribir(ENTER, out);
                esperaOk(in);           
                mensaje = obtenerPantalla(OBTENER_PANTALLA_PRESS, out, in);
                escribir(ENTER, out);
                esperaOk(in);
               break;
            case NO_CONFIRMAR_DECISION:
                escribir(NEGAR, out);
                esperaOk(in);
                escribir(ENTER, out);
                esperaOk(in);         
                mensaje = obtenerPantalla(OBTENER_PANTALLA_PRESS, out, in);
                escribir(ENTER, out);
                esperaOk(in);
            break;
       }
       return mensaje;
    }
    
    public String AddTask(String numero, String cabecera, String descripcion, String dia, String mes, String anyio) throws IOException, InterruptedException{ 
        escribir(ELEGIR_ADD_TASK, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(INICIO_MENSAJE_MAINFRAME + Integer.parseInt(numero) + FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(INICIO_MENSAJE_MAINFRAME+ cabecera +FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(INICIO_MENSAJE_MAINFRAME+ descripcion +FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(INICIO_MENSAJE_MAINFRAME+ Integer.parseInt(dia) +FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(INICIO_MENSAJE_MAINFRAME+ Integer.parseInt(mes) +FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(INICIO_MENSAJE_MAINFRAME+ Integer.parseInt(anyio) +FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        return TAREA_ANYADIDA_CORRECTAMENTE;
    }
    
    public String RemoveTask(int numeroTarea)throws IOException, InterruptedException{
        String pantalla = null;      
        switch (pasoRemoveTask){
            case 0:
                escribir(ELEGIR_REMOVE_TASK, out);
                esperaOk(in);
                escribir(ENTER, out);
                esperaOk(in);
                pantalla = obtenerPantalla(OBTENER_PANTALLA_REMOVE, out, in);
                System.out.println(pantalla);
                pasoRemoveTask++;
            break;
            case 1:
                escribir(INICIO_MENSAJE_MAINFRAME+ numeroTarea +FINAL_MENSAJE_MAINFRAME, out);
                esperaOk(in);
                escribir(ENTER, out);
                esperaOk(in);
                pantalla = obtenerPantalla(OBTENER_PANTALLA_CONFIRM, out, in);
                System.out.println(pantalla);
                pasoRemoveTask = 0;
            break;
        }
        return pantalla;
    }
    public String confirmarRemoveTask(int decision)throws IOException, InterruptedException{  
        String pantalla = null;
        if(decision == CONFIRMAR_DECISION){ 
            escribir(CONFIRM, out);
            esperaOk(in);
            escribir(ENTER, out);
            esperaOk(in);
        }else{
            escribir(NEGAR, out);
            esperaOk(in);
            escribir(ENTER, out);
            esperaOk(in);
        }
        pantalla = obtenerPantalla(OBTENER_PANTALLA_PRESS, out, in);
        System.out.println(pantalla);
        escribir(ENTER, out);
        esperaOk(in);
        return pantalla;
    
    }
     
    public String searchTask(int diaTask, int mesTask, int anyioTask)throws IOException, InterruptedException{
        String respuesta = null;
        escribir(ELEGIR_SEARCH_TASK, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);    
        escribir(INICIO_MENSAJE_MAINFRAME+ diaTask +FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);  
        escribir(INICIO_MENSAJE_MAINFRAME+ mesTask +FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(INICIO_MENSAJE_MAINFRAME+ anyioTask +FINAL_MENSAJE_MAINFRAME, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        respuesta = obtenerPantalla(OBTENER_PANTALLA_PRESS, out, in);
        escribir(ENTER, out);
        esperaOk(in);
        return respuesta;
    }
      
    public String listTask()throws IOException, InterruptedException{
        String respuesta = null;
        escribir(ELEGIR_LIST_TASK, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        respuesta = obtenerPantalla(OBTENER_PANTALLA_LIST, out, in);
        escribir(ENTER, out);
        esperaOk(in);
        return respuesta;
    }
    
    public String saveTask()throws IOException, InterruptedException{
        String respuesta = null;
        escribir(ELEGIR_SAVE_TASK, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        respuesta = obtenerPantalla(OBTENER_PANTALLA_PRESS, out, in);
        escribir(ENTER, out);
        esperaOk(in);
        return respuesta;
    }
      
    public String salirAplicacion() throws IOException, InterruptedException{
        escribir(ELEGIR_SALIR_APLICACION, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        obtenerPantalla(BYE, out, in);     
        escribir(ENTER, out);
        esperaOk(in);
        obtenerPantalla(COMMAND, out, in);           
        escribir(OFF, out);
        esperaOk(in);
        escribir(ENTER, out);
        esperaOk(in);
        escribir(DESCONECTADO, out); 
        esperaOk(in);   
        escribir(QUIT, out);
        esperaOk(in);   
        in.close();
        out.close();
        s3270.destroy();
        return MENSAJE_SALIR_APLICACION;
    }   
  public void nuevoObservador(PropertyChangeListener observador) {
    this.observadores.addPropertyChangeListener(observador);
  }
}
