--  Based on scikit-learn/sklearn/utils.multiclass.py

with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Printing;

with Encode_Utils;

package body Multiclass_Utils is

   --  -------------------------------------------------------------------------
   --  L118
   function Is_Multilabel (Y : Integer_Array) return Boolean is
      use Ada.Containers;
      --          Routine_Name : constant String := "Multiclass_Utils.Is_Multilabel array ";
      Labels : constant NL_Types.Integer_List := Unique_Labels (Y);
   begin
      return Labels.Length > 2;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------
   --  L118
   function Is_Multilabel (Y : NL_Types.Integer_List) return Boolean is
      use Ada.Containers;
      --          Routine_Name : constant String :=
      --                           "Multiclass_Utils.Is_Multilabel Integer_List ";
      Labels : constant NL_Types.Integer_List := Unique_Labels (Y);
   begin
      return Labels.Length > 2;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------
   --  L118
   --  Examples:
   --  is_multilabel ([0, 1, 0, 1]) = False
   --  is_multilabel ([[1, 0], [0, 0]]) = True
   --  is_multilabel ([[1], [0], [0]]) = False
   --  is_multilabel ([[1, 0, 0]]) = True
   function Is_Multilabel (Y : Integer_Matrix) return Boolean is
      use Ada.Containers;
      --          Routine_Name : constant String := "Multiclass_Utils.Is_Multilabel matrix ";
      Labels       : NL_Types.Integer_List;
      Multilabel   : Boolean := Y'Length (2) > 1;
   begin
      if Multilabel then
         Labels := Unique_Labels (Y);
         Multilabel := Labels.Length < 3;
      end if;

      return Multilabel;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------

   function Is_Multilabel (Y : NL_Types.Integer_List_Array) return Boolean is
      use Ada.Containers;
      --          Routine_Name : constant String := "Multiclass_Utils.Is_Multilabel matrix ";
      Labels       : NL_Types.Integer_List;
      Multilabel   : Boolean := False;
   begin
      for row in Y'Range loop
         if Y (row).Length > 1 then
            Multilabel := True;
         end if;
      end loop;

      if Multilabel then
         Labels := Unique_Labels (Y);
         Multilabel := Labels.Length < 3;
      end if;

      return Multilabel;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------
   --  L118
   function Is_Multilabel (Y : Real_Float_Matrix) return Boolean is
      use Ada.Containers;
      Labels     : NL_Types.Float_List;
      Multilabel : Boolean := Y'Length (2) > 1;
   begin
      if Multilabel then
         Labels := Unique_Labels (Y);
         Multilabel := Labels.Length < 3;
      end if;

      return Multilabel;

   end Is_Multilabel;

   --  -------------------------------------------------------------------------

   --  L202
   function Type_Of_Target (Y : Boolean_Matrix) return Y_Type is
      pragma Unreferenced (Y);
   begin
      return Y_Binary;
   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : Integer_Array) return Y_Type is
      use Ada.Containers;
      Classes : constant NL_Types.Integer_List := Unique_Labels (Y);
      Result  : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Classes.Length > 2 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------

   function Type_Of_Target (Y : NL_Types.Integer_List) return Y_Type is
      use Ada.Containers;
      Classes : constant NL_Types.Integer_List := Unique_Labels (Y);
      Result  : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Classes.Length > 2 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------

   function Type_Of_Target (Y : NL_Types.Integer_List_Array) return Y_Type is
      use Ada.Containers;
      Classes : constant NL_Types.Integer_List := Unique_Labels (Y);
      Result  : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Classes.Length > 2 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : Integer_Matrix) return Y_Type is
      --        Routine_Name : constant String :=
      --                           "Multiclass_Utils.Type_Of_Target Integer_Matrix ";
      Result : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Y'Length (2) > 1 then
         Result := Y_Multiclass_Multioutput;
      elsif Y'Length > 1 then
         Result := Y_Multiclass;
      else
         Result := Y_Binary;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  L202
   function Type_Of_Target (Y : Real_Float_Matrix) return Y_Type is
      Result : Y_Type;
   begin
      if Is_Multilabel (Y) then
         Result := Y_Multilabel_Indicator;
      elsif Y'Length (2) > 1 then
         Result := Y_Continuous_Multioutput;
      else
         Result := Y_Continuous;
      end if;

      return Result;

   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  unique_labels L101
   --     function Unique_Multiclass_Set (Y : Integer_Matrix)
   --                                     return Encode_Utils.Int_Sets.Set is
   --     begin
   --        --  L22 _unique_multiclass, return np.unique
   --        return Encode_Utils.Unique (Y);
   --
   --     end Unique_Multiclass_Set;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Real_Float_Matrix) return NL_Types.Float_List is
   begin
      --  L101
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Integer_Array) return NL_Types.Integer_List is
   begin
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : NL_Types.Integer_List)
                            return NL_Types.Integer_List is
   begin
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Integer_Matrix) return NL_Types.Integer_List is
      --        --  L84
      --        Label_Kind   : Label_Type := Unique_Binary;
      --        Label_Set    : Encode_Utils.Int_Sets.Set;
   begin
      --  L101
      --        case Label_Kind is
      --           --  L35
      --           when Unique_Binary | Unique_Multiclass =>
      --              --  L22
      --              Label_Set := Unique_Multiclass_Set (Y);
      --              --  L29
      --           when Unique_Mutilabel_Indicator => Null;
      --              Labels := Unique_Indicator (Y);
      --        end case;
      --  L111
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : NL_Types.Integer_List_Array)
                            return NL_Types.Integer_List is
   begin
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L101
   --     function Unique_Label_Set (Y : Integer_Matrix)
   --                                return Encode_Utils.Int_Sets.Set is
   --        Labels       : Encode_Utils.Int_Sets.Set;
   --        --  L84
   --        Label_Kind   : Label_Type := Unique_Binary;
   --     begin
   --        case Label_Kind is
   --           --  L35
   --           when Unique_Binary | Unique_Multiclass =>
   --              --  L22
   --              Labels := Unique_Multiclass_Set (Y);
   --              --  L29
   --           when Unique_Mutilabel_Indicator => Null;
   --  --              Labels := Unique_Indicator (Y);
   --        end case;
   --
   --        return Labels;
   --
   --     end Unique_Label_Set;

   --  -------------------------------------------------------------------------

end Multiclass_Utils;
