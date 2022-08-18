--  Based on https://github.com/PabloSaavedra/
--  Adaline-Neuronal-Network/blob/master/SCX1.PAS

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;

with Maths;

package body Connectionist is

    type Neural_Node_Class (Num_Inputs, Num_Weights : Positive) is record
        Inputs    : Real_Float_Vector (1 .. Num_Inputs);
        Weights   : Real_Float_Vector (1 .. Num_Weights);
        Activ     : Float;            --  activation signal
        output    : Float;            --  exit
        Tendencon : Boolean;          --  indicates if the trend is active
        Trend     : Float;            --  trend value}
        Trained   : Boolean := False; --  the neuron is trained
    end record;

    package Data_Values_Package is new
      Ada.Containers.Vectors (Positive, Float);
    subtype Data_Value_List is Data_Values_Package.Vector;

    package Neural_Net_Package is new
      Ada.Containers.Indefinite_Vectors (Positive, Neural_Node_Class);
    subtype Neural_Net_List is Neural_Net_Package.Vector;

    Max_Data     : constant Positive := 10;
    Max_Layers : constant Positive := 10;

    subtype Data is  Real_Float_Vector (1.. Max_Data);
    type Layer is array (1.. Max_Data) of Data;
    subtype Output is Real_Float_Vector (1.. Max_Data);

    Float_Gen : Ada.Numerics.Float_Random.Generator;

    Learning_Rate  : Float;
    Sail_Learn     : Float := 0.025;
    Max_Error      : Float := 0.0001;  --  maximum allowed error
--      Neuron        : Neural_Node_Class;
    Neural_Net     : Neural_Net_List;
    --    input_Layers : tpatrentr; { array of input Layers
    --    outputd  : toutput; { array of desired outputs
    Active_Layer : Boolean := False; -- Training Layer loaded
    Num_Data       : Natural := 0; --  number of data (neuron inputs, data per Layer }
    Num_Layers   : Natural := 0;  --  number of training Layers }
    --
    --    fichpesos : tfichtxt; { file of weights }
    --    fileerror : tfichtxt; { file of errors }
    --    nfichpesos : string; { name of weights file }
    --    nficherror : string; { error file name }
    --
    Error_Recorded : Boolean := False;  --  EMCs are recorded }
    Weights_Recorded : Boolean := False; --  successive weights are saved }
    --    key : char; { option menu key }
    Program_End : Boolean := False;

    procedure Adaline (Num_Inputs : Positive) is
        use Ada.Numerics.Float_Random;
        --  Num_Inputs + 1 to allow for a bias
        theAdaline : Adaline_Class (Num_Inputs + 1);
    begin
        theAdaline.Num_Inputs := Num_Inputs;
        for index in 1 .. Num_Inputs + 1 loop
            theAdaline.Weights (index) := Float (Random (Float_Gen));
        end loop;

    end Adaline;

    --  ---------------------------------------------------------------------------------------------

    procedure Adaline (Weights : Real_Float_Vector) is
        use Ada.Numerics.Float_Random;
        theAdaline : Adaline_Class (Weights'Length - 1);
    begin
        theAdaline.Num_Inputs := Weights'Length - 1;
        theAdaline.Weights := Weights;

    end Adaline;

    --  ---------------------------------------------------------------------------------------------

    procedure Adjust_Weights (theAdaline : in out Adaline_Class;
                              Input        : Real_Float_Vector;
                              Learning_Rate : Float) is
    begin
        null;

    end Adjust_Weights;

    --  ---------------------------------------------------------------------------------------------

    function Evaluate (theAdaline : Adaline_Class;
                       Input : Real_Float_Vector) return Float is
    begin
        return 0.0;

    end Evaluate;

    --  ---------------------------------------------------------------------------------------------

    procedure Run_Adaline is
    begin
        null;
    end Run_Adaline;

    --  ---------------------------------------------------------------------------------------------

    procedure Train (theAdaline    : in out Adaline_Class;
                     Input         : Real_Float_Vector; Desired,
                     Learning_Rate : Float) is
        Raw             : Float := Evaluate (theAdaline, input);
        Error           : Float := Desired - Raw;
        Learning_Factor : Float := Error * Learning_Rate;
    begin
        Adjust_Weights (theAdaline, Input, Learning_Rate);

    end Train;

    --  ---------------------------------------------------------------------------------------------

end Connectionist;
