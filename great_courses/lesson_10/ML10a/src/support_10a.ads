
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;

package Support_10A is

   type Features_Record is record
      P_Class  : Positive;
      Sex      : Natural;
      Age      : Float;
      Sib_Sp   : Natural;
      Parch    : Natural;
      Fare     : Float;
      Embark_S : Boolean := False;
      Embark_C : Boolean := False;
      Embark_Q : Boolean := False;
   end record;

   type Features_Array is array (Integer range <>) of Features_Record;

   type Data_Record (Num_Items : Positive) is record
      Features      : Features_Array (1 .. Num_Items);
      Labels        : Integer_Array (1 .. Num_Items);
      Feature_Names : Unbounded_String_Array (1 .. 9) :=
                        (To_Unbounded_String ("Pclass"), To_Unbounded_String ("Sex"),
                         To_Unbounded_String ("Age"), To_Unbounded_String ("SibSp"),
                         To_Unbounded_String ("Parch"), To_Unbounded_String ("Fare"),
                         To_Unbounded_String ("Embarked S"),
                         To_Unbounded_String ("Embarked C"),
                         To_Unbounded_String ("Embarked Q"));
   end record;

   type Split_Data_Record (Num_Train, Num_Test : Positive) is record
      Train_Features : Features_Array (1 .. Num_Train);
      Train_Labels   : Integer_Array (1 .. Num_Train);
      Test_Features  : Features_Array (1 .. Num_Test);
      Test_Labels    : Integer_Array (1 .. Num_Test);
   end record;

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : Python_API.PyObject; A : Features_Array;
                   B   : Integer_Array);
   function Get_Data (File_Name : String) return Data_Record;
   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float;
   pragma Inline (Error);
   function Get_Split_Data (File_Name : String) return Split_Data_Record;

end Support_10A;
