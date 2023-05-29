
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;

package Support_16A is

   type Data_Items (Rows : Positive) is record
      Features : Integer_Array_List;
      Labels   : Integer_Matrix (1 .. Rows, 1 .. 1);
   end record;

   type Dictionary_Record is record
      Key   : Unbounded_String;
      Value : Natural := 0;
   end record;

   type Newsgroups_Record is record
      Data         : Python_API.PyObject;
      Target       : Python_API.PyObject;
      File_Names   : Python_API.PyObject;
      Descr        : Python_API.PyObject;
      Target_Names : Python_API.PyObject;
   end record;

   package Dictionary_Package is new
     Ada.Containers.Doubly_Linked_Lists (Dictionary_Record);
   subtype Dictionary_List is Dictionary_Package.List;

   function Call_Python (M : Python.Module; Function_Name : String)
                         return Newsgroups_Record;
   function ProbA_Chooser
     (Classifier       : Python.Module;
      Current_Item     : Positive; Num_Items : Positive;
      Labeled_Examples : Data_Items;
      Train_Set        : ML_Types.Integer_List_2D;
      Train_Labels     : ML_Types.Integer_List; Alpha : Integer;
      Clf              : Python_API.PyObject) return Integer;
   function Get_Data (File_Name : String) return Data_Items;
   function Read_Vocabulary (File_Name : String) return Dictionary_List;
   procedure Save_Data (Data : Newsgroups_Record; File_Name : String);

end Support_16A;
