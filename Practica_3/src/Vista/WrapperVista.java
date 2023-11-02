
package Vista;
import Modelo.WrapperX3270Music;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.BoxLayout;
import javax.swing.JScrollPane;
import javax.swing.JTextField;


public class WrapperVista implements ActionListener, PropertyChangeListener {
    private final String MENSAJE_ELIMINAR_TAREA = "ESCRIBE EL NUMERO DE LA TAREA A ELIMINAR ";
    private final String MENSAJE_EMERGENTE_ELIMINAR_TAREA = "¿SEGURO QUE QUIERES ELIMINAR ESTA TAREA?";
    private final String VACIO = "" ;
    private final String CONECTAR = "CONECTAR";
    private final String USERID = "USERID";
    private final String PASSWORD = "PASSWORD";
    private final String DATA = "data:";
    private final String NEW_TASK_FILE = "NEW TASK FILE";  
    private final String ADD_TASK = "ADD TASK"; 
    private final String REMOVE_TASK = "REMOVE TASK"; 
    private final String SEARCH_TASK = "SEARCH TASK"; 
    private final String LIST_TASK = "LIST TASKS";
    private final String SAVE_TASK = "SAVE TASKS";
    private final String EXIT = "EXIT";    
    private final String ACEPTAR_NUEVA_TAREA = "ACEPTAR";
    private final String NEGAR_NUEVA_TAREA = "NEGAR";
    private final String ENVIAR_ANYADIR_TAREA = "ENVIAR";
    private final String ENVIAR_ELIMINAR_TAREA = "ENVIAR ";
        
    /*ELEMENTOS GRÁFICOS USADOS EN LA VISTA DEL WRAPPER*/
    private static WrapperVista instancia = null;
    public WrapperX3270Music wrapperX3270Music;
    
    //Pantalla principal
    private JTextArea userId;        
    private JTextArea password;       
    private JTextField introducirUserId;          
    private JTextField introducirPassword; 
    private JTextArea informacionAplicacion;
    private JButton conectar;
    private JButton boton1 ;
    private JButton boton2;
    private JButton boton3;
    private JButton boton4;
    private JButton boton5;  
    private JButton boton6;
    private JButton boton7;
    private JPanel panelNorteVentanaPrincipal;
    private JPanel panelCentralVentanaPrincipal;
    private JPanel panelEsteVentanaPrincipal;
    private JFrame ventanaPrincipal;
    
    //New Task
    private JButton botonAceptarNewTaskFile;
    private JButton botonNegarNewTaskFile;
    private JFrame ventanaNuevaTarea;
    private JTextArea pantallaNuevaTarea;
    private JPanel panelSuperiorNuevaTarea;
    private JPanel panelInferiorNuevaTarea;
    
    //Add Task
    private JPanel panelCentralAnyadirTarea;
    private JPanel panelInferiorAnyadirTarea;
    private JFrame ventanaAnyadirTarea; 
    private JButton botonEnviarAnyadirTarea;
    private JTextArea numeroAnyadirTarea;
    private JTextArea cabeceraAnyadirTarea;
    private JTextArea descripcionAnyadirTarea;
    private JTextArea diaAnyadirTarea;
    private JTextArea mesAnyadirTarea;
    private JTextArea anyioAnyadirTarea;
    private JTextField introducirNumeroAnyadirTarea;
    private JTextField introducirCabeceraAnyadirTarea;
    private JTextField introducirDescripcionAnyadirTarea;
    private JTextField introducirDiaAnyadirTarea;
    private JTextField introducirMesAnyadirTarea;
    private JTextField introducirAnyoAnyadirTarea;
    private final String TEXTO_NUMERO_ADD_TASK = "Añade un numero a la tarea";
    private final String TEXTO_CABECERA_ADD_TASK = "Añade una cabecera a la tarea";
    private final String TEXTO_TEXTO_ADD_TASK = "Añade un texto a la tarea";
    private final String TEXTO_DIA_ADD_TASK = "Añade un dia a la tarea";
    private final String TEXTO_MES_ADD_TASK = "Añade un mes a la tarea";
    private final String TEXTO_ANYO_ADD_TASK = "Añade un año a la tarea";
    
    //Remove Task
    private JTextField introducirNumeroEliminarTarea;
    private JPanel panelCentralEliminarTarea ;
    private JTextArea pantallaEliminarTarea ;
    private JFrame ventanaEliminarTarea;
    private JButton botonEnviarEliminarTarea ;
    private JScrollPane scrollRemoveTarea;
    private final String TEXTO_ELIMINAR_TASK = "Escriba el número de la tarea a eliminar";
    
    private JPanel panelCentralVentanaEmergenteEliminarTarea ;
    private JTextArea pantallaVentanaEmergenteEliminarTarea;
    private JFrame ventanaVentanaEmergenteEliminarTarea;
    private JButton botonConfirmarEliminarTarea;
    private JButton botonNegarEliminarTarea;
    private final String TEXTO_BOTON_CONFIRMAR_REMOVE_TASK = "Eliminar";
    private final String TEXTO_BOTON_CANCELAR_REMOVE_TASK = "Cancelar";
    
    //Search Task
    private JPanel panelSuperiorBuscarTarea;
    private JPanel panelCentralBuscarTarea;
    private JFrame ventanaBuscarTarea;
    private JButton botonEnviarBuscarTarea; 
    private JTextField introducirDiaBuscarTarea;
    private JTextField introducirMesBuscarTarea;
    private JTextField introducirAnyioBuscarTarea;
    private JTextArea diaBuscarTarea;
    private JTextArea mesBuscarTarea;
    private JTextArea anyioBuscarTarea;
    private JTextArea pantallaBuscarTarea;
    private JScrollPane scrollBuscarTarea;
    private final String TEXTO_DIA_SEARCH_TASK = "Escrbe un día";
    private final String TEXTO_MES_SEARCH_TASK = "Escrbe un mes";
    private final String TEXTO_ANYO_SEARCH_TASK = "Escrbe un año";
    private final String ENVIAR_BUSCAR_TAREA = "Buscar tarea";
    
    //List Task
    private JPanel panelCentralListarTareas;
    private JTextArea pantallaListarTareas;
    private JFrame ventanaListarTareas;
    private JScrollPane scrollListarTareas;
    
    //Save Task
    private JPanel panelCentralGuardarTareas;
    private JTextArea pantallaGuardarTareas ;
    private JFrame ventanaGuardarTareas;
   

    public WrapperVista( WrapperX3270Music wrapperX3270Music)
            throws IOException{       
        this.wrapperX3270Music = wrapperX3270Music;        
        crearVentanaPrincipal();   
    }
  
         
    public static synchronized WrapperVista devolverInstancia( WrapperX3270Music wrapperX3270Music)
                             throws IOException {
        if (instancia == null){
            instancia = new WrapperVista(wrapperX3270Music);
        }
            return instancia;
    } 
      
    private void crearVentanaPrincipal(){
        ventanaPrincipal = new JFrame();
        panelNorteVentanaPrincipal = new JPanel();
        panelCentralVentanaPrincipal = new JPanel();     
        panelEsteVentanaPrincipal = new JPanel();
    
        ventanaPrincipal.setMinimumSize(new Dimension(800, 300));   
        ventanaPrincipal.getContentPane().setLayout(new BorderLayout()); 
        panelNorteVentanaPrincipal.setLayout(new FlowLayout());
        
        panelCentralVentanaPrincipal.setLayout(new FlowLayout());
        panelEsteVentanaPrincipal.setLayout(new BoxLayout(panelEsteVentanaPrincipal, BoxLayout.PAGE_AXIS));
  
        ventanaPrincipal.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }   
        });    
        creaMenuInicioSesion(panelNorteVentanaPrincipal);
        ventanaPrincipal.getContentPane().add(panelNorteVentanaPrincipal,
                                              BorderLayout.NORTH);
        creaPanelCentral(panelCentralVentanaPrincipal);
        ventanaPrincipal.getContentPane().
                         add(panelCentralVentanaPrincipal, 
                         BorderLayout.CENTER);
        
        crearBotonesPantallaPrincipal(panelEsteVentanaPrincipal);
        ventanaPrincipal.getContentPane().
                         add(panelEsteVentanaPrincipal, 
                         BorderLayout.EAST);

        ventanaPrincipal.setResizable(false);    
        ventanaPrincipal.pack();
        ventanaPrincipal.setVisible(true);
        ventanaPrincipal.setLocationRelativeTo(null);
    }
  
  
    public void creaMenuInicioSesion(JPanel panelNorte) {
        userId = new JTextArea(USERID);
        userId.setEditable(false);        
        password = new JTextArea(PASSWORD);
        password.setEditable(false);        
        introducirUserId = new JTextField(20);              
        introducirPassword = new JTextField(20);       
        conectar = crearBoton(CONECTAR);
      
        panelNorte.add(userId);
        panelNorte.add(introducirUserId);
        panelNorte.add(password);
        panelNorte.add(introducirPassword); 
        panelNorte.add(conectar);
    } 
    private void crearBotonesPantallaPrincipal(JPanel panel){
        
        boton1 = crearBoton(NEW_TASK_FILE);
        boton2 = crearBoton(ADD_TASK);
        boton3 = crearBoton(REMOVE_TASK);
        boton4 = crearBoton(SEARCH_TASK);
        boton5 = crearBoton(LIST_TASK);  
        boton6 = crearBoton(SAVE_TASK);
        boton7 = crearBoton(EXIT);
        
        boton1.setVisible(false);
        boton2.setVisible(false);
        boton3.setVisible(false);
        boton4.setVisible(false);
        boton5.setVisible(false);
        boton6.setVisible(false);
        boton7.setVisible(false);
        
        panel.add(boton1);
        panel.add(boton2);
        panel.add(boton3);
        panel.add(boton4);
        panel.add(boton5);
        panel.add(boton6);
        panel.add(boton7);
    }
  
   
    private JButton crearBoton(String etiqueta) {
        JButton boton = new JButton(etiqueta);
        boton.addActionListener(this);
        boton.setActionCommand(etiqueta);
        return boton;
    }  
  
    private void creaPanelCentral(JPanel panel) {
      informacionAplicacion = new JTextArea();     
      panel.add(informacionAplicacion);
    }
       
  public  String  parsearInformacionMainframe(String oracion,String palabra) {
    if(oracion.contains(palabra))
        return oracion.replaceAll(palabra, "");
    return oracion;
}
  public void crearVentanaNuevaTarea() throws IOException, InterruptedException{
    panelSuperiorNuevaTarea = new JPanel();
    panelInferiorNuevaTarea = new JPanel();
    pantallaNuevaTarea = new JTextArea();
    ventanaNuevaTarea = new JFrame();
    botonAceptarNewTaskFile = crearBoton(ACEPTAR_NUEVA_TAREA);
    botonNegarNewTaskFile = crearBoton(NEGAR_NUEVA_TAREA);

    ventanaNuevaTarea.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                ventanaNuevaTarea.setVisible(false);
            }   
        });  
    ventanaNuevaTarea.getContentPane().setLayout(new BorderLayout());
    pantallaNuevaTarea.setText( parsearInformacionMainframe(wrapperX3270Music.NewTaskFile(0), DATA));
    
    panelSuperiorNuevaTarea.setLayout(new GridLayout(2, 2));
    panelSuperiorNuevaTarea.add(pantallaNuevaTarea);
    ventanaNuevaTarea.getContentPane().add(panelSuperiorNuevaTarea, BorderLayout.CENTER);
    
    panelInferiorNuevaTarea.setLayout(new GridLayout(2, 2));
    panelInferiorNuevaTarea.add(botonAceptarNewTaskFile);
    panelInferiorNuevaTarea.add(botonNegarNewTaskFile); 
    ventanaNuevaTarea.getContentPane().add(panelInferiorNuevaTarea, BorderLayout.NORTH);
   
    ventanaNuevaTarea.setResizable(false);    
    ventanaNuevaTarea.pack();  
    ventanaNuevaTarea.setVisible(true);
    ventanaNuevaTarea.setLocationRelativeTo(null);
  }
  
  public void crearVentanaAnyadirTarea () throws IOException, InterruptedException{        
    
    panelCentralAnyadirTarea = new JPanel();
    panelInferiorAnyadirTarea = new JPanel();
    ventanaAnyadirTarea = new JFrame();
    botonEnviarAnyadirTarea = new JButton();
    
    numeroAnyadirTarea = new JTextArea(TEXTO_NUMERO_ADD_TASK);
    cabeceraAnyadirTarea = new JTextArea(TEXTO_CABECERA_ADD_TASK);
    descripcionAnyadirTarea = new JTextArea(TEXTO_TEXTO_ADD_TASK);
    diaAnyadirTarea = new JTextArea(TEXTO_DIA_ADD_TASK);
    mesAnyadirTarea = new JTextArea(TEXTO_MES_ADD_TASK);
    anyioAnyadirTarea = new JTextArea(TEXTO_ANYO_ADD_TASK);
    
    introducirNumeroAnyadirTarea = new JTextField(3);
    introducirCabeceraAnyadirTarea = new JTextField(50);
    introducirDescripcionAnyadirTarea = new JTextField(200);
    introducirDiaAnyadirTarea = new JTextField(2);
    introducirMesAnyadirTarea = new JTextField(2);
    introducirAnyoAnyadirTarea = new JTextField(4);
    
    botonEnviarAnyadirTarea = crearBoton(ENVIAR_ANYADIR_TAREA);

    ventanaAnyadirTarea.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                ventanaAnyadirTarea.setVisible(false);
            }   
        });  
    ventanaAnyadirTarea.getContentPane().setLayout(new BorderLayout());
   
    panelCentralAnyadirTarea.setLayout(new GridLayout(2, 2));
    //Numero
    panelCentralAnyadirTarea.add(numeroAnyadirTarea);
    panelCentralAnyadirTarea.add(introducirNumeroAnyadirTarea);
    //Cabecera
    panelCentralAnyadirTarea.add(cabeceraAnyadirTarea);
    panelCentralAnyadirTarea.add(introducirCabeceraAnyadirTarea);
    //Descripcion
    panelCentralAnyadirTarea.add(descripcionAnyadirTarea);
    panelCentralAnyadirTarea.add(introducirDescripcionAnyadirTarea);
    //Dia
    panelCentralAnyadirTarea.add(diaAnyadirTarea);
    panelCentralAnyadirTarea.add(introducirDiaAnyadirTarea);
    //Mes
    panelCentralAnyadirTarea.add(mesAnyadirTarea);
    panelCentralAnyadirTarea.add(introducirMesAnyadirTarea);
    //Anyio
    panelCentralAnyadirTarea.add(anyioAnyadirTarea);
    panelCentralAnyadirTarea.add(introducirAnyoAnyadirTarea);
    ventanaAnyadirTarea.getContentPane().add(panelCentralAnyadirTarea, BorderLayout.CENTER);
    
    panelInferiorAnyadirTarea.setLayout(new GridLayout());
    panelInferiorAnyadirTarea.add(botonEnviarAnyadirTarea);
    ventanaAnyadirTarea.getContentPane().add(panelInferiorAnyadirTarea, BorderLayout.SOUTH);
   
    ventanaAnyadirTarea.setResizable(false);    
    ventanaAnyadirTarea.pack(); 
    ventanaAnyadirTarea.setVisible(true);
    ventanaAnyadirTarea.setLocationRelativeTo(null);
  }
  
  
  public void crearVentanaRemoveTarea () throws IOException, InterruptedException{        
    panelCentralEliminarTarea = new JPanel();
    pantallaEliminarTarea = new JTextArea(MENSAJE_ELIMINAR_TAREA);  
    ventanaEliminarTarea = new JFrame();
    botonEnviarEliminarTarea = new JButton(); 
    introducirNumeroEliminarTarea = new JTextField();
    botonEnviarEliminarTarea = crearBoton(ENVIAR_ELIMINAR_TAREA);
    
 
    ventanaEliminarTarea.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                ventanaEliminarTarea.setVisible(false);
            }   
        });  
    ventanaEliminarTarea.getContentPane().setLayout(new BorderLayout());
    
    panelCentralEliminarTarea.setLayout(new GridLayout());
    panelCentralEliminarTarea.add(pantallaEliminarTarea);
    panelCentralEliminarTarea.add(introducirNumeroEliminarTarea);
    panelCentralEliminarTarea.add(botonEnviarEliminarTarea); 
    ventanaEliminarTarea.getContentPane().add(panelCentralEliminarTarea, BorderLayout.CENTER);
    
    ventanaEliminarTarea.setResizable(false);    
    ventanaEliminarTarea.pack(); 
    ventanaEliminarTarea.setVisible(true);
    ventanaEliminarTarea.setLocationRelativeTo(null);
    wrapperX3270Music.RemoveTask(0);
  }
  
  private void ventanaEmergenteEliminarTarea() throws IOException, InterruptedException{

    panelCentralVentanaEmergenteEliminarTarea = new JPanel();
    pantallaVentanaEmergenteEliminarTarea = new JTextArea(MENSAJE_EMERGENTE_ELIMINAR_TAREA);
    ventanaVentanaEmergenteEliminarTarea = new JFrame();
    
    botonConfirmarEliminarTarea = new JButton();
    botonNegarEliminarTarea = new JButton();
    botonConfirmarEliminarTarea = crearBoton(TEXTO_BOTON_CONFIRMAR_REMOVE_TASK);
    botonNegarEliminarTarea = crearBoton(TEXTO_BOTON_CANCELAR_REMOVE_TASK);
    
    ventanaVentanaEmergenteEliminarTarea.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                ventanaVentanaEmergenteEliminarTarea.setVisible(false);
            }   
        });  
    ventanaVentanaEmergenteEliminarTarea.getContentPane().setLayout(new BorderLayout());
    
    panelCentralVentanaEmergenteEliminarTarea.setLayout(new GridLayout(2, 2));
    panelCentralVentanaEmergenteEliminarTarea.add(pantallaVentanaEmergenteEliminarTarea);
    panelCentralVentanaEmergenteEliminarTarea.add(botonConfirmarEliminarTarea);
    panelCentralVentanaEmergenteEliminarTarea.add(botonNegarEliminarTarea);
    
    ventanaVentanaEmergenteEliminarTarea.getContentPane().add(panelCentralVentanaEmergenteEliminarTarea, BorderLayout.CENTER);
 
    ventanaVentanaEmergenteEliminarTarea.setResizable(false);    
    ventanaVentanaEmergenteEliminarTarea.pack(); 
    ventanaVentanaEmergenteEliminarTarea.setVisible(true);
    ventanaVentanaEmergenteEliminarTarea.setLocationRelativeTo(null);
    wrapperX3270Music.RemoveTask(Integer.parseInt(introducirNumeroEliminarTarea.getText()));
    }
  
  public void crearVentanaBuscarTarea() throws IOException, InterruptedException{       
    panelSuperiorBuscarTarea = new JPanel();
    panelCentralBuscarTarea = new JPanel();
    pantallaBuscarTarea = new JTextArea();
    ventanaBuscarTarea = new JFrame();
    scrollBuscarTarea = new JScrollPane(pantallaBuscarTarea);
    botonEnviarBuscarTarea = new JButton(); 
    introducirDiaBuscarTarea = new JTextField();
    introducirMesBuscarTarea = new JTextField();
    introducirAnyioBuscarTarea = new JTextField();
    diaBuscarTarea = new JTextArea(TEXTO_DIA_SEARCH_TASK);
    mesBuscarTarea = new JTextArea(TEXTO_MES_SEARCH_TASK);
    anyioBuscarTarea = new JTextArea(TEXTO_ANYO_SEARCH_TASK);
    botonEnviarBuscarTarea = crearBoton(ENVIAR_BUSCAR_TAREA);
    scrollBuscarTarea.setPreferredSize(new Dimension(450, 230));
 
    ventanaBuscarTarea.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                ventanaBuscarTarea.setVisible(false);
            }   
        });  
    ventanaBuscarTarea.getContentPane().setLayout(new BorderLayout());
    
    panelSuperiorBuscarTarea.setLayout(new GridLayout());
    panelSuperiorBuscarTarea.add(diaBuscarTarea);
    panelSuperiorBuscarTarea.add(introducirDiaBuscarTarea); 
    
    panelSuperiorBuscarTarea.add(mesBuscarTarea); 
    panelSuperiorBuscarTarea.add(introducirMesBuscarTarea);
    
    panelSuperiorBuscarTarea.add(anyioBuscarTarea);
    panelSuperiorBuscarTarea.add(introducirAnyioBuscarTarea); 
    panelSuperiorBuscarTarea.add(botonEnviarBuscarTarea); 
    ventanaBuscarTarea.getContentPane().add(panelSuperiorBuscarTarea, BorderLayout.NORTH);
    
    panelCentralBuscarTarea.setLayout(new GridLayout());
    panelCentralBuscarTarea.add(scrollBuscarTarea);
    ventanaBuscarTarea.getContentPane().add(panelCentralBuscarTarea, BorderLayout.CENTER);
    
    ventanaBuscarTarea.setResizable(false);    
    ventanaBuscarTarea.pack(); 
    ventanaBuscarTarea.setVisible(true);
    ventanaBuscarTarea.setLocationRelativeTo(null);
  }
  
  public void crearVentanaListarTareas() throws IOException, InterruptedException{     
    panelCentralListarTareas = new JPanel();
    pantallaListarTareas = new JTextArea();
    ventanaListarTareas = new JFrame();
    scrollListarTareas = new JScrollPane(pantallaListarTareas);
    scrollListarTareas.setPreferredSize(new Dimension(450, 230));
    
    ventanaListarTareas.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                ventanaListarTareas.setVisible(false);
            }   
        });  
    ventanaListarTareas.getContentPane().setLayout(new BorderLayout());
    
    panelCentralListarTareas.setLayout(new GridLayout(2, 2));
    panelCentralListarTareas.add(scrollListarTareas);
    ventanaListarTareas.getContentPane().add(panelCentralListarTareas, BorderLayout.CENTER);
    ventanaListarTareas.setResizable(false);    
    ventanaListarTareas.pack(); 
    ventanaListarTareas.setVisible(true);
    ventanaListarTareas.setLocationRelativeTo(null);
    rellenarScrollListarTareas();
  }
  private void rellenarScrollListarTareas() throws IOException, InterruptedException{
      pantallaListarTareas.setText(wrapperX3270Music.listTask());
  }
  private void buscarTareas(String dia, String mes, String anyo) {  
      try {
           pantallaBuscarTarea.setText( parsearInformacionMainframe(wrapperX3270Music.searchTask(Integer.parseInt(dia), Integer.parseInt(mes), Integer.parseInt(anyo)), DATA));
      } catch (IOException ex) {
            Logger.getLogger(WrapperVista.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(WrapperVista.class.getName()).log(Level.SEVERE, null, ex);
        }    
  }
  
  private void listarTareas() throws IOException, InterruptedException{
        wrapperX3270Music.listTask();
  }
    
    public void actionPerformed(ActionEvent e)  {
        try {      
            notificacionAControl(e.getActionCommand());
        }catch (IOException ex) {
            Logger.getLogger(WrapperVista.class.getName()).
                             log(Level.SEVERE,null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(WrapperVista.class.getName()).log(Level.SEVERE, null, ex);
        }
    } 
    
     private void guardarTareas() throws IOException, InterruptedException{
        panelCentralGuardarTareas = new JPanel();
        pantallaGuardarTareas = new JTextArea();
        ventanaGuardarTareas = new JFrame();
        pantallaGuardarTareas.setPreferredSize(new Dimension(450, 230));
        ventanaGuardarTareas.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                ventanaGuardarTareas.setVisible(false);
            }   
        });  
        ventanaGuardarTareas.getContentPane().setLayout(new BorderLayout());
    
        panelCentralGuardarTareas.setLayout(new GridLayout(2, 2));
        panelCentralGuardarTareas.add(pantallaGuardarTareas);
        ventanaGuardarTareas.getContentPane().add(panelCentralGuardarTareas, BorderLayout.CENTER);

        ventanaGuardarTareas.setResizable(false);    
        ventanaGuardarTareas.pack(); 
        ventanaGuardarTareas.setVisible(true);
        ventanaGuardarTareas.setLocationRelativeTo(null);
    
        pantallaGuardarTareas.setText(parsearInformacionMainframe(wrapperX3270Music.saveTask(), DATA));    
    }
 
   private void salirAplicacion(){
        try {
           informacionAplicacion.setText(wrapperX3270Music.salirAplicacion());
        } catch (IOException ex) {
            Logger.getLogger(WrapperVista.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(WrapperVista.class.getName()).log(Level.SEVERE, null, ex);
        }
        boton1.setVisible(false);
        boton2.setVisible(false);
        boton3.setVisible(false);
        boton4.setVisible(false);
        boton5.setVisible(false);
        boton6.setVisible(false);
        boton7.setVisible(false);
   }
    private void conectarMainframe(){
        try {
            if (wrapperX3270Music.conectarMainframe()){
                wrapperX3270Music.login(introducirUserId.getText(), introducirPassword.getText());
            }
                    } catch (IOException ex) {
            Logger.getLogger(WrapperVista.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(WrapperVista.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void notificacionAControl(String evento) throws IOException, InterruptedException{
        switch(evento) {
            case CONECTAR:
                conectarMainframe();
                break;                
            case NEW_TASK_FILE :
                crearVentanaNuevaTarea();
                break;                  
            case ADD_TASK:    
                crearVentanaAnyadirTarea(); 
                break;                   
            case REMOVE_TASK:
                crearVentanaRemoveTarea();
                break;                 
            case SEARCH_TASK:     
                crearVentanaBuscarTarea();            
                break;              
            case LIST_TASK:
              crearVentanaListarTareas();
                break;  
            case SAVE_TASK:
              guardarTareas();
                break; 
            case EXIT:
              salirAplicacion();
                break; 
            case ACEPTAR_NUEVA_TAREA:
                 aceptarNuevaTarea();
                break;
            case NEGAR_NUEVA_TAREA:
                 negarNuevaTarea();
                break;
            case ENVIAR_ANYADIR_TAREA:
                enviarAnyadirTarea();
            break;
            
            case ENVIAR_ELIMINAR_TAREA:
            ventanaEmergenteEliminarTarea();
            break;
            
            case TEXTO_BOTON_CONFIRMAR_REMOVE_TASK:
                confirmarEliminarTarea();
            break;
            
            case TEXTO_BOTON_CANCELAR_REMOVE_TASK:
                cancelarEliminarTarea();
            break;      
            
            case ENVIAR_BUSCAR_TAREA:
                buscarTareas(introducirDiaBuscarTarea.getText(), introducirMesBuscarTarea.getText(), introducirAnyioBuscarTarea.getText());
            break;        
        }
    }
    private void confirmarEliminarTarea() throws IOException, InterruptedException{
        wrapperX3270Music.confirmarRemoveTask(1);
    }
    
    private void cancelarEliminarTarea() throws IOException, InterruptedException{
        wrapperX3270Music.confirmarRemoveTask(2);
    }
     
    private void enviarAnyadirTarea() throws IOException, InterruptedException{
        
        wrapperX3270Music.AddTask(introducirNumeroAnyadirTarea.getText(),
                                  introducirCabeceraAnyadirTarea.getText(),
                                  introducirDescripcionAnyadirTarea.getText(),
                                  introducirDiaAnyadirTarea.getText(),
                                  introducirMesAnyadirTarea.getText(),
                                  introducirAnyoAnyadirTarea.getText());  
    }
    private void negarNuevaTarea() throws InterruptedException, IOException{
        pantallaNuevaTarea.setText( parsearInformacionMainframe(wrapperX3270Music.NewTaskFile(2), DATA));
    }
    private void aceptarNuevaTarea() throws IOException, InterruptedException{
        pantallaNuevaTarea.setText( parsearInformacionMainframe(wrapperX3270Music.NewTaskFile(1), DATA));
    }
    @Override
    public void propertyChange(PropertyChangeEvent pce) {
    }
}