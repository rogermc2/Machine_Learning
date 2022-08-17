program sistemasconexionistas;
program connectionist systems;
const
  maxdata    = 10;
  maxpatrons = 10;
type

  tdata = array [1..maxdata] of real;        { tipo para entries y weights en la neuron }
  tpatrentr = array [1..maxpatrons] of tdata;{ tipo para Save los patrones de entrada }
  tsalidad = array [1..maxpatrons] of real;   { tipo para Save las salidas deseadas }

  ppe = ^tpe;  { Puntero PE }
  tpe = record  { tipo pe = elemento de procesado }
        entr      : tdata;    {array de entries}
        weight      : tdata;    {array de weights}
        vactiv    : real;      {se�al de activacion}
        salida    : real;      {salida}
        tendencon : boolean;   {indica si la tendencia esta activa}
        tendencia : real;      {valor de la tendencia}
        trained : boolean;   {la neuron esta trained}
        enlace    : ppe;       {enlace con el siguiente PE}
  end;{tpe}

  preal = ^treal;           { Dato para crear una lista de reales }
  treal = record            { en la que se Savean los valores  }
          dato: real;       { successives de la variacion de weights  }
          enlace : preal;   { en funcion del tiempo.              }
  end;

  tfichtxt = text;

var
  learning_speed        : real;      { speed de learning }
  maxerror         : real;      { error maximum permited }
  neuron          : tpe;       { una neuron }
  patrentr         : tpatrentr; { array de patrones de entrada }
  salidad          : tsalidad;  { array de salidas deseadas }
  patronactivo     : boolean;   { dice si hay un patron de training cargado }
  NumDatos         : integer;   { numero de datos(entries neuron,datos por patron }
  NumPatrones      : integer;   { numero de patrones de training }

  fichweights        : tfichtxt;  { file de weights }
  ficherror        : tfichtxt;  { file de errors }
  nfichweights       : string;    { nombre file de weights }
  nficherror       : string;    { nombre file de errors }

  record_error       : boolean;   { dice si se graban los EMC }
  grab_weights       : boolean;   { dice si se graban los weights successives }
  tecla            : char;      { tecla para el menu de opciones }
  fin              : boolean;   { fin de programa }

{***************************************************************************}
procedure inicvar;
  begin
    learning_speed:=0.025;
    maxerror:=0.0001;
    NumDatos    :=0;
    NumPatrones :=0;
    record_error:=false;
    grab_weights:=false;
  end;
{***************************************************************************}
procedure calcsalida(var neuron : tpe);
{ calcula la salida de la neuron }
  procedure calcvactiv(var neuron : tpe);
  { calcula el valor de activacion }
    function neto (neuron : tpe): real;
    { calcula el valor neto }
      var i     : integer;
          valor : real;
      begin
        valor:=0;
        for i:=1 to numdatos do begin
          valor:=valor+(neuron.entr[i]*neuron.weight[i]);
        end;
        if neuron.tendencon then valor:=valor+neuron.tendencia;
        neto:=valor;
      end;
    begin
      neuron.vactiv:=neto(neuron);
    end;
  begin
    calcvactiv(neuron);
    neuron.salida:=neuron.vactiv;
  end;
{***************************************************************************}
procedure inicneuron( var neuron : tpe );
  var i : integer;
  begin
    for i:=1 to numdatos do begin
      neuron.weight[i]:=random(3);
      neuron.entr[i]:=0;
    end;
    neuron.tendencia:=random(3);
    neuron.vactiv:=0;
    neuron.salida:=0;
    neuron.trained:=false;
  end;
{***************************************************************************}
procedure intro_training;
  var
      tecla : char;
  {-------------------------------------}
  procedure introducirpatrones;
    var i,j : integer;
    begin
      write('Enter numero de patrones: ');readln(numpatrones);
      write('Enter numero entries : ');readln(numdatos);
      for i:=1 to numpatrones do begin
        for j:=1 to numdatos do begin
          write('Enter patron ',i:2,', dato ',j:2,' : ');
          readln(patrentr[i,j]);
        end;
        write('Enter salida deseada : ');
        readln(salidad[i]);
      end;
      patronactivo:=true;
    end;
  {-------------------------------------}
  procedure read_patronesfich;
    var  i,j : integer;
    begin
      write('Enter nombre del file: ');
      readln(nfichweights);
      assign(fichweights,nfichweights);
      reset(fichweights);
      readln(fichweights,numpatrones);
      readln(fichweights,numdatos);
      for i:=1 to numpatrones do begin
        for j:=1 to numdatos do begin
          readln(fichweights,patrentr[i,j]);
        end;
        readln(fichweights,salidad[i]);
      end;
      close(fichweights);
      patronactivo:=true;
    end;
  {-------------------------------------}
  procedure leerweightsfich;
    var i : integer;
    begin
      write('Enter nombre del file: ');
      readln(nfichweights);
      assign(fichweights,nfichweights);
      reset(fichweights);
      readln(fichweights,numdatos);
      for i:=1 to numdatos do begin
        read(fichweights,neuron.weight[i]);
      end;
      close(fichweights);
      patronactivo:=true;
      neuron.trained:=true;
      writeln('training no realizado.');
      writeln('weights asignados a la neuron');
    end;
  {-------------------------------------}
  procedure recordpatrfich;
    var  i,j : integer;
    begin
      write('Enter nombre del file: ');
      readln(nfichweights);
      assign(fichweights,nfichweights);
      rewrite(fichweights);
      writeln(fichweights,numpatrones);
      writeln(fichweights,numdatos);
      for i:=1 to numpatrones do begin
        for j:=1 to numdatos do begin
          writeln(fichweights,patrentr[i,j]);
        end;
        writeln(fichweights,salidad[i]);
      end;
      close(fichweights);
    end;
  {-------------------------------------}
  begin
    writeln('** change PATRON DE training **');
    writeln('         [1] introducir patrones manualmente.');
    writeln('         [2] leer patrones de file.');
    writeln('         [3] leer weights ya calculados y no hacer training');
    writeln('         [4] record patron de training a file');
    writeln('         [0] volver al menu principal.');
    write('  opcion:');readln(tecla);
    case tecla of
       '1':introducirpatrones;
       '2':read_patronesfich;
       '3':leerweightsfich;
       '4':recordpatrfich;
    end;
  end;

{***************************************************************************}
procedure train_neuron;
  var
    fintraining : boolean;   { el training ha acabado }
    interacciones    : integer;   { numero de interacciones en training }
    emc              : real;      { error medio cuadratico }
    errors          : tsalidad;  { array de errors }
    error_actual     : real;      { error al aplicar el patron actual }
    opcion           : char;
    cpatron          : integer;   { contador de patrones de entrada }
    i,j,k,l          : integer;
    nombrfich        : array [1..maxdata] of string;
    listareal        : preal;     { lista donde Saveemos los weights en el tiempo }
    valor            : real;      { valor que tomamos de la lista }
  {-------------------------------------}
    procedure calcemc;
      var i : integer;
       begin
         for i:=1 to numpatrones do begin
           emc:=emc+errors[i];
         end;
         emc:=emc/numpatrones;
       end;
  {-------------------------------------}
    procedure ponerpatron ( var neuron : tpe ;patron : tdata ;salidad : real );
                                          { patron que le llega, salida deseada}
      var i : integer;
      begin
        for i:=1 to numdatos do begin
          neuron.entr[i]:=patron[i];
        end;
        {calculamos la salida}
        calcsalida(neuron);
        error_actual:=salidad-neuron.salida;
        errors[cpatron]:=sqr(error_actual);
      end;
  {-------------------------------------}
    procedure change_weights(var neuron : tpe);
      var i : integer;
      begin
        for i:=1 to numdatos do begin
          neuron.weight[i]:=neuron.weight[i]+2*learning_speed*error_actual*neuron.entr[i];
        end;
        if neuron.tendencon then
           neuron.tendencia:=neuron.tendencia+2*learning_speed*error_actual;
      end;
  {-------------------------------------}
    procedure iniclista(var lista : preal);
      begin
        lista:=nil;
      end;
  {-------------------------------------}
    procedure meterenlista(var lista : preal;dato : real);
      var buscafin : preal;
          new : preal;
      begin
        new(new);
        new^.dato:=dato;
        new^.enlace:=nil;
        if lista=NIL then
           lista:=new
        else begin
           buscafin:=lista;
           while buscafin^.enlace<>nil do begin
             buscafin:=buscafin^.enlace;
           end;
           buscafin^.enlace:=new;
        end;
      end;
  {-------------------------------------}
    function cogerdatolista(lista : preal;pos : integer):real;
      var i : integer;
      begin
        if lista<>NIL then begin
           for i:=2 to pos do
             lista:=lista^.enlace;
           cogerdatolista:=lista^.dato;
        end;
      end;
  {------ e n t r e n a m i e n t o ----}
  begin
    if not patronactivo then begin
      writeln('no hay ningun patron de training cargado.');
    end else begin
      if record_error then begin
         write('Enter nombre del file de EMC : ');
         readln(nficherror);
         assign(ficherror,nficherror);
         rewrite(ficherror);
      end;
      inicneuron(neuron);
      if grab_weights then begin
         for i:=1 to NumDatos do begin
           write('Enter nombre del file de weights Entrada ',i,' : ');
           readln(nombrfich[i]);
         end;
         iniclista(listareal);
         for i:=1 to NumDatos do
           meterenlista(listareal,neuron.weight[i]);
      end;
      write('  quiere tendencia (s/n) ? ');
      readln(opcion);
      if opcion='s' then neuron.tendencon:=true else neuron.tendencon:=false;
      interacciones:=0;
      emc:=0;
      fintraining:=false;
      writeln('entrenando neuron.');
      while not fintraining do begin
        cpatron:=1;
        while cpatron<=numpatrones do begin
          ponerpatron(neuron,patrentr[cpatron],salidad[cpatron]);
          change_weights(neuron);
          cpatron:=cpatron+1;
        end;
        interacciones:=interacciones+1;
        calcemc;
        writeln('en la interaccion ',interacciones,' el emc ha sido ',emc);
        if record_error=true then write(ficherror,emc);
        if grab_weights then begin
           for i:=1 to NumDatos do
             meterenlista(listareal,neuron.weight[i]);
        end;
        if emc<maxerror then fintraining:=true;
        emc:=0;
      end;
      neuron.trained:=true;
      writeln('neuron trained.');
      writeln('los weights son : w1=',neuron.weight[1],';  w2=',neuron.weight[2]);
      tecla:='a';
      if grab_weights then begin
         k:=3;
         for i:=1 to numdatos do begin
           l:=k;
           assign(fichweights,nombrfich[i]);
           rewrite(fichweights);
           for j:=1 to interacciones do begin
             valor:=cogerdatolista(listareal,l);
             write(fichweights,valor);
             l:=l+numdatos;
           end;
           close(fichweights);
           k:=k+1;
         end;
      end;
      if record_error then close(ficherror);
    end;
  end;
{***************************************************************************}
procedure probarneuron;
  var i : integer;
  begin
    if neuron.trained then begin
      for i:=1 to numdatos do begin
        write('Enter el dato ',i,': ');
        readln(neuron.entr[i]);
      end;
      calcsalida(neuron);
      writeln ('salida =',neuron.salida);
    end else begin
      writeln('la neuron no ha sido trained');
    end;
  end;
{***************************************************************************}
procedure Saveweights;
  var i : integer;
  begin
    write('Enter nombre del file: ');
    readln(nfichweights);
    assign(fichweights,nfichweights);
    rewrite(fichweights);
    writeln(fichweights,numdatos);
    for i:=1 to numdatos do begin
      write(fichweights,neuron.weight[i]);
    end;
    close(fichweights);
  end;
{***************************************************************************}
procedure verpatrentr;
  var i,j : integer;
  begin
    for i:=1 to numpatrones do begin
      write('patron ',i:2,': ');
      for j:=1 to numdatos do begin
        write(patrentr[i,j]:5:3,',');
      end;
      writeln(' -> salida deseada: ',salidad[i]:5:3);
    end;
  end;
{***************************************************************************}
procedure Enter_learning_speed;
  begin
    write('Enter new learning speed: ');
    readln(learning_speed);
  end;
{***************************************************************************}
procedure intromaxerror;
  begin
    write('Enter new error maximum: ');
    readln(maxerror);
  end;
{********************** p r i n c i p a l **********************************}
begin
  fin:=false;
  patronactivo:=false;
  inicvar;
  while not fin do begin
    writeln;
    writeln('**** Practica de Sistemas Conexionistas por Pablo Saavedra Lopez ****');
    writeln('Menu Principal : ');
    writeln('         [1] train neuron');
    writeln('         [2] probar la neuron');
    writeln('         [3] Save weights neuron en file');
    writeln('         [4] Choose patrones training');
    writeln('         [5] ver patron de training actual');
    writeln('         [6] change speed de learning ',learning_speed:7:5);
    writeln('         [7] change error maximum permited ',maxerror:7:5);
    writeln('         [8] record EMC successive a file = ',record_error);
    writeln('         [9] record weights successives a file = ',grab_weights);
    writeln('         [0] Leave.');
    write('  opcion:');readln(tecla);
    case tecla of
       '1':train_neuron;
       '2':probarneuron;
       '3':Saveweights;
       '4':intro_training;
       '5':verpatrentr;
       '6':Enter_learning_speed;
       '7':intromaxerror;
       '8':record_error:=not(record_error);
       '9':grab_weights:=not(grab_weights);
       '0':fin:=true;
    end;
  end;
end.