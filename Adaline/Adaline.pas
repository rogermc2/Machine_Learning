

program connectionist_systems; 
const 
  maxdata = 10; 
  maxpatterns = 10; 
type 

  tdata = array [1..maxdata] of real; { type for inputs and weights in the neuron } 
  tpatrentr = array [1..maxpatterns] of tdata;{ type to store input patterns } 
  toutput = array [1..maxpatterns] of real; { type to save the desired outputs } 

  ppe = ^tpe; { PE pointer } 
  tpe = record { type pe = processing element } 
        entr : tdata; {array of inputs} 
        weight : tdata; {array of weights} 
        vactiv : real; {activation signal} 
        output : real; {exit}
        tendencon : boolean; {indicates if the trend is active} 
        trend : real; {trend value} 
        trained : boolean; {the neuron is trained} 
        link : ppe; {link to next PE} 
  end;{tpe} 

  preal = ^treal; { Data to create a list of reals } 
  treal = record { in which the values ​​will be saved } 
          data: real; { successive of the variation of weights } 
          link: preal; { function of time. } 
  end; 

  tfichtxt = text; 

var 
  learning_rate : actual; { learning rate } 
  maxerror : real; { maximum allowed error }
  neuron: tpe; { a neuron } 
  patrentr : tpatrentr; { array of input patterns } 
  outputd : toutput; { array of desired outputs } 
  activepattern : boolean; { tells if there is a training pattern loaded } 
  NumData : integer; { number of data (neuron inputs, data per pattern } 
  NumPatterns : integer; { number of training patterns } 

  fichpesos : tfichtxt; { file of weights } 
  fileerror : tfichtxt; { file of errors } 
  nfichpesos : string; { name of weights file } 
  nficherror : string; { error file name } 

  recordaerror : boolean; { says if the EMCs are recorded }
  recordweights : boolean; { says if successive weights are saved } 
  key : char; { option menu key } 
  end : boolean; { end of program } 

{******************************************** *******************************} 
procedure initvar; 
  begin 
    saillearn:=0.025; 
    maxerror:=0.0001; 
    NumData :=0; 
    NumPatterns :=0; 
    recorderror:=false; 
    recordweights:=false; 
  end; 
{***************************************************** **************************} 
procedure calcoutput(var neuron : tpe); 
{ calculate the output of the neuron } 
  procedure calcvactiv(var neuron : tpe);
  { calculate activation value } 
    net function (neuron : tpe): real; 
    { compute net worth } 
      var i : integer; 
          real value; 
      begin 
        value:=0; 
        for i:=1 to numdata do begin 
          value:=value+(neuron.entr[i]*neuron.weight[i]); 
        end; 
        if neuron.trendcon then value:=value+neuron.trend; 
        net:=value; 
      end; 
    begin 
      neuron.vactiv:=net(neuron); 
    end; 
  begin 
    calcvactiv(neuron); 
    neuron.output:=neuron.vactiv; 
  end;
{***************************************************** **************************} 
procedure inicneuron( var neuron : tpe ); 
  var i : integer; 
  begin 
    for i:=1 to numdata do begin 
      neuron.weight[i]:=random(3); 
      neuron.entr[i]:=0; 
    end; 
    neuron.trend:=random(3); 
    neuron.vactiv:=0; 
    neuron.output:=0; 
    neuron.trained:=false; 
  end; 
{***************************************************** **************************} 
procedure introtraining; 
  var 
      key : char; 
  {-------------------------------------} 
  procedure introducepatterns; 
    var i,j : integer;
    begin 
      write('enter number of patterns: '); readln(numpatterns); 
      write('enter number of entries: '); readln(numdata); 
      for i:=1 to numpatterns do begin 
        for j:=1 to numdata do begin 
          write('enter pattern ',i:2,', data ',j:2,' : '); 
          readln(patrentr[i,j]); 
        end; 
        write('enter desired output: '); 
        readln(output[i]); 
      end; 
      activepattern:=true; 
    end; 
  {-------------------------------------} 
  procedure readpatternsfile; 
    var i,j : integer; 
    begin 
      write('enter file name: ');
      readln(nfichpesos);
      assign(fichpesos,nfichpesos);
      reset(fichpesos);
      readln(fichpesos,numpatrones);
      readln(fichpesos,numdatos);
      for i:=1 to numpatrones do begin
        for j:=1 to numdatos do begin
          readln(fichpesos,patrentr[i,j]);
        end;
        readln(fichpesos,salidad[i]);
      end;
      close(fichpesos);
      patronactivo:=true;
    end;
  {-------------------------------------}
  procedure readweightsfile;
    var i : integer;
    begin
      write('introduzca nombre del fichero: ');
      readln(nfichpesos);
      assign(fichpesos,nfichpesos); 
      reset(fichpesos); 
      readln(fichpesos, numdata); 
      for i:=1 to numdata do begin 
        read(fichpesos,neurona.peso[i]); 
      end; 
      close(fichpesos); 
      activepattern:=true; 
      neuron.trained:=true; 
      writeln('training not done.'); 
      writeln('weights assigned to the neuron'); 
    end; 
  {-------------------------------------} 
  procedure save filepattern; 
    var i,j : integer; 
    begin 
      write('enter file name: '); 
      readln(nfichweights); 
      assign(fichpesos,nfichpesos);
      rewrite(fichpesos); 
      writeln(weightfile, numpatterns); 
      writeln(fichpesos,numdata); 
      for i:=1 to numpatrones do begin 
        for j:=1 to numdata do begin 
          writeln(fichpesos,patrentr[i,j]); 
        end; 
        writeln(fichpesos,out[i]); 
      end; 
      close(fichpesos); 
    end; 
  {-------------------------------------} 
  begin 
    writeln('** CHANGE TRAINING PATTERN * *'); 
    writeln(' [1] enter patterns manually.'); 
    writeln(' [2] read patterns from file.'); 
    writeln(' [3] read weights already calculated and do not train');
    writeln(' [4] write training pattern to file'); 
    writeln(' [0] return to main menu.'); 
    write(' option:'); readln(key); 
    case key of 
       '1': enter patterns; 
       '2':readpatternsfile; 
       '3':readweightsfile; 
       '4': save filepattern; 
    end; 
  end; 

{***************************************************** **************************} 
procedure trainneuron; 
  var 
    endtraining : boolean; { training is over } 
    interactions : integer; { number of interactions in training } 
    emc : real;
    errors : tout; { array of errors } 
    current_error : real; { error applying current pattern } 
    option : char; 
    cpattern : integer; { input pattern counter } 
    i,j,k,l : integer; 
    filename : array [1..MaxData] of string; 
    royallist : preal; { list where we will store the weights in time } 
    value : real; { value we take from the list } 
  {-------------------------------------} 
    procedure calcemc; 
      var i : integer; 
       begin 
         for i:=1 to numpatterns do begin 
           emc:=emc+errors[i]; 
         end;
         emc:=emc/numpatterns; 
       end; 
  {-------------------------------------} 
    procedure setpattern ( var neuron : tpe ;pattern : tdata ;out : real ); 
                                          { received pattern, desired output} 
      var i : integer; 
      begin 
        for i:=1 to numdata do begin 
          neuron.entr[i]:=pattern[i]; 
        end; 
        {calculate the output} 
        calcoutput(neuron); 
        current_error:=neuron-output.output; 
        errors[cpattern]:=sqr(current_error); 
      end; 
  {-------------------------------------} 
    procedure changeweights(var neuron : tpe);
      var i : integer; 
      begin 
        for i:=1 to numdata do begin 
          neuron.weight[i]:=neuron.weight[i]+2*candlelearn*error_current*neuron.entr[i]; 
        end; 
        if neuron.trendcon then 
           neuron.trend:=neuron.trend+2*saillearn*current_error; 
      end; 
  {-------------------------------------} 
    procedure iniclista(var list : preal); 
      begin 
        list:=nil; 
      end; 
  {-------------------------------------} 
    procedure putenlist(var list : preal;data : real ); 
      var finseeker : preal; 
          new : preal; 
      begin 
        new(new);
        new^.data:=data; 
        new^.link:=nil; 
        if list=NIL then 
           list:=new 
        else begin 
           searchend:=list; 
           while findend^.link<>nil do begin 
             findend:=findsee^.link; 
           end; 
           searchend^.link:=new; 
        end; 
      end; 
  {-------------------------------------} 
    function getlistdata(list : preal;pos : integer) :real; 
      var i : integer; 
      begin 
        if list<>NIL then begin 
           for i:=2 to pos do 
             list:=list^.link; 
           getlistdata:=list^.data;
        end; 
      end; 
  {------ training ----} 
  begin 
    if not activepattern then begin 
      writeln('There is no training pattern loaded.'); 
    end else begin 
      if writeerror then begin 
         write('enter name of EMC file: '); 
         readln(nerrorfile); 
         assign(errorfile,nerrorfile); 
         rewrite(errorfile); 
      end; 
      inichneuron(neuron); 
      if recordweights then begin 
         for i:=1 to NumData do begin 
           write('enter weights file name Input ',i,' : '); 
           readln(filename[i]); 
         end;
         iniclist(reallist); 
         for i:=1 to NumData do 
           putinlist(reallist,neuron.weight[i]); 
      end; 
      write(' want trend (y/n) ? '); 
      readln(option); 
      if option='s' then neuron.tendencon:=true else neuron.tendencon:=false; 
      interactions:=0; 
      emc:=0; 
      endtraining:=false; 
      writeln('training neuron.'); 
      while not endtraining do begin 
        cpattern:=1; 
        while cpattern<=numpatterns do begin 
          setpattern(neuron,pattern[cpattern],output[cpattern]); 
          changeweights(neuron); 
          cpattern:=cpattern+1;
        end; 
        interactions:=interactions+1; 
        calcemc; 
        writeln('in the interaction ',interactions,' the emc has been ',emc); 
        if writeerror=true then write(errorfile,emc); 
        if recordweights then begin 
           for i:=1 to NumData do 
             putinlist(reallist,neuron.weight[i]); 
        end; 
        if emc
   
     salida deseada: ',salidad[i]:5:3);
    end;
  end;
{***************************************************************************}
procedure introlearning_rate;
  begin
    write('introduzca nueva velocidad de aprendizaje: ');
    readln(learning_rate);
  end;
{***************************************************************************}
procedure intromaxerror;
  begin
    write('introduzca nuevo error maximo: ');
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
    writeln('         [1] entrenar neurona');
    writeln('         [2] probar la neurona');
    writeln('         [3] guardar pesos neurona en fichero');
    writeln('         [4] elegir patrones entrenamiento');
    writeln('         [5] ver patron de entrenamiento actual');
    writeln('         [6] cambiar velocidad de aprendizaje ',learning_rate:7:5);
    writeln('         [7] cambiar error maximo permitido ',maxerror:7:5);
    writeln('         [8] grabar EMC sucesivo a fichero = ',grabaerror);
    writeln(' [9] record successive weights to file = ',recordweights); 
    writeln(' [0] exit.'); 
    write(' option:'); readln(key); 
    case key of 
       '1':trainneuron; 
       '2':testneuron; 
       '3':saveweights; 
       '4': introtraining; 
       '5':verpatrentr; 
       '6':introvelalearn; 
       '7':intromaxerror; 
       '8':recorderror:=not(recorderror); 
       '9':recordweights:=not(recordweights); 
       '0':end:=true; 
    end; 
  end; 
end.

   