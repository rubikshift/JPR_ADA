--LEGENDA:
	--Otwieramy calkowice nowy sklep z androidami!
	--Przygotowywujemy nasze modele na specjalne zamowienie naszych klientow.
	--Niestety nie mamy jeszcze zbyt wielu pracownikow
	--i mozemy obslugiwac tylko niewielu klientow na raz.
	--Cierpliwosc klientow jest rozna, czasem wycofuja sie za zamowienia
	--zanim jeszcze zaczniemy ich obslugiwac :(
	--W zwiazku z eksluzywnoscia naszych towarow
	--nowe czesci przyjmujemy tylko, w sytuacjach:
		--a) nie mamy czesci na magazynie, wtedy nic sie nie zmarnuje
		--b) nie mamy czesci potrzebnych do skompletowania obecnych zamowien
	--Niestety nie mozemy sobie pozowlic na straty, nie kupujemy czesci, 
	--jezeli zaczyna nam brakowac miejsca w magazynie a mamy czesci potrzebne 
	--do wyprodukowania obecnych zamowien, albo co gorsza nie mamy zamowien.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; 
with Ada.Numerics.Discrete_Random;


procedure Symulacja is
	DifferentPartsNumber:		constant Integer := 3;
	DifferentModelsNumber:		constant Integer := 3;
	DifferentCustomersNumber:	constant Integer := 4;
	
	subtype DurationOfProduction	is Integer range 3 .. 6;
	subtype DurationOfConsumption	is Integer range 4 .. 8;
	subtype PatienceOfCustomer		is Integer range 1 .. 10;
	
	subtype PartType		is Integer range 1 .. DifferentPartsNumber;
	subtype ModelType		is Integer range 1 .. DifferentModelsNumber;
	subtype CustomerType	is Integer range 1 .. DifferentCustomersNumber;
	
	PartName:	constant array (PartType) of String(1 .. 15) := ("Cz. Mechaniczne", "Cz. Organiczne ", "J. Obliczeniowa");
	ModelName:	constant array (ModelType) of String(1 .. 7) := ("Nexus-6", "C3PO <3", "Bishop1");
	CustomerName: constant array (1 .. DifferentCustomersNumber) of String(1 .. 12) := ("L. Skywalker", "Rick Deckard", "Riley Ripley", "Student Infy");
	
	package RandomConsumptionTime is new Ada.Numerics.Discrete_Random(DurationOfConsumption);
	package RandomPatience is new Ada.Numerics.Discrete_Random(PatienceOfCustomer);
	package RandomModelOrder is new Ada.Numerics.Discrete_Random(ModelType);
	package RandomProductionTime is new Ada.Numerics.Discrete_Random(DurationOfProduction);

	task type Producer is
		entry Start(Part: in PartType);
		end Producer;
	
	task type Customer is
		entry Start(ID: in CustomerType);
		entry ReciveOrderedModel(OrderedModel: in ModelType; SerialNumber: in Integer);
		end Customer;
	
	task type AndroidShop is
		entry AcceptOrder(CustomerID: in CustomerType; OrderedModel: in ModelType);
		entry ReciveParts(Part: in PartType; SerialNumber: in Integer; Recived: out Boolean);
		end AndroidShop;
	
	P: array ( 1 .. DifferentPartsNumber ) of Producer;
	K: array ( 1 .. DifferentCustomersNumber ) of Customer;
	B: AndroidShop;

	task body Producer is
		G: RandomProductionTime.Generator;
		ProducedPartType: Integer;
		PartSerialNumber: Integer;
		Delivered: Boolean := False;

		begin
			accept Start(Part: in PartType) do
				RandomProductionTime.Reset(G);
				PartSerialNumber := 1;
				ProducedPartType := Part;
    		end Start;
    
			Put_Line("Producent " & PartName(ProducedPartType) & " rozpoczyna swoja dzialalnosc!");
    
			loop
				delay Duration(RandomProductionTime.Random(G));
				Put_Line("Wyprodukowano " & PartName(ProducedPartType) & " S/N"  & Integer'Image(PartSerialNumber));
				while not Delivered loop					-- Jak sklep nie przyjmie, to za chwile sprobujemy wyslac ponownie juz wyprodukowana czesc, moze juz potrzebuja
					Put_Line("Wyslano oferte dostawy " & PartName(ProducedPartType) & " S/N"  & Integer'Image(PartSerialNumber));
					B.ReciveParts(ProducedPartType, PartSerialNumber, Delivered);
					delay 5.0;
				end loop;
				Delivered := False;
				PartSerialNumber := PartSerialNumber + 1;
    		end loop;
		end Producer;

	task body Customer is
		G: RandomConsumptionTime.Generator;
   		G2: RandomModelOrder.Generator;
		G3: RandomPatience.Generator;
		CustomerID: CustomerType;	
		OrderedModel: Integer;
		WaitnigForOrder: Boolean := False;
			
		begin
			accept Start(ID: in CustomerType) do
				RandomConsumptionTime.Reset(G);
				RandomModelOrder.Reset(G2);
				RandomPatience.Reset(G3);
				CustomerID := ID;
			end Start;
	
			Put_Line("Pojawil sie nowy potencjalny klient: " & CustomerName(CustomerID));

			loop
				select
					accept ReciveOrderedModel(OrderedModel: in ModelType; SerialNumber: in Integer) do
						Put_Line(CustomerName(CustomerID) & ": Otrzymalem sowje zamowienie: " & ModelName(OrderedModel) & " S/N " & Integer'Image(SerialNumber));
						WaitnigForOrder := False;
					end ReciveOrderedModel;
				else
					if not WaitnigForOrder then
						delay Duration(RandomConsumptionTime.Random(G));
						OrderedModel := RandomModelOrder.Random(G2);
						Put_Line(CustomerName(CustomerID) & ": Chce zamowic androida model: " & ModelName(OrderedModel));
						select
							B.AcceptOrder(CustomerID, OrderedModel);
							WaitnigForOrder := True;
						or delay Duration(RandomPatience.Random(G3));
							Put_Line(CustomerName(CustomerID) & ": Slaby sklep, 2/10, za dlugi czas oczekiwania na przyjecie zamowienia!");
						end select;
					end if;
				end select;
				delay 1.0;
			end loop;
		end Customer;
	
	task body AndroidShop is
		StoreCapacity: constant Integer := 15;
		type Typ_Magazynu is array (PartType) of Integer;
		type Order is
			record
				CustomerID: CustomerType;
				ModelID: 	ModelType;
				Pending:	Boolean;
			end record;
		Magazyn: Typ_Magazynu := (0, 0, 0);
		PartsRequiredForModel: array(ModelType, PartType) of Integer := ((0, 2, 2), (2, 0, 2), (1, 2, 1));
		MaxOrders: constant Integer := 3;
		subtype OrderID is Integer range 1 .. MaxOrders;
		Orders: array(1 .. MaxOrders) of Order;
		OrdersCount: Integer := 0;
		ModelSerialNumber: array(ModelType) of Integer := (1, 1, 1);
		OnStore: Integer := 0;
		
		function CanRecive(Part: PartType) return Boolean is
			TempStore: Typ_Magazynu;		--  magazyn po przyjęciu części
			RequiredPartsCount: Integer := 0;
			begin
				TempStore := Magazyn;
				TempStore(Part) := TempStore(Part) + 1;
				if  StoreCapacity - OnStore > StoreCapacity / 2 then 		-- Ponad polowa magazynu wolna = mozna przyjmowac
					return True;
				elsif OrdersCount > 0 then									-- Sprawdzamy na co jest zapotrzebowanie
					for Z in 1 .. OrdersCount loop
						RequiredPartsCount := RequiredPartsCount + PartsRequiredForModel(Orders(Z).ModelID, Part);
					end loop;
					if RequiredPartsCount - TempStore(Part) > 0 then
						return True;
					else
						return False;
					end if;
				else														-- Nie ma zmowien, w magazynie robi sie tloczno, nie mozemy ryzykowac
					return False;
				end if;
			end CanRecive;
		
		function CanDeliver(Zestaw: ModelType) return Boolean is
			begin
				for W in PartType loop
					if Magazyn(W) < PartsRequiredForModel(Zestaw, W) then
						return False;
					end if;
				end loop;
				return True;
			end CanDeliver;
		
		procedure PrintStore is
			begin
				Put_Line("SKLEP: Sklad magazynu: ");
				for W in PartType loop
					Put_Line(Integer'Image(W) & ". " & PartName(W) & ":" & Integer'Image(Magazyn(W)));
				end loop;
			end PrintStore;
		procedure PrintOrders is
			begin
				Put_Line("SKLEP: Oczekujace zamowienia");
				for i in 1 .. OrdersCount loop
					Put_Line(Integer'Image(i) & ". " & CustomerName(Orders(i).CustomerID) & ": " & ModelName(Orders(i).ModelID));
				end loop;
			end PrintOrders;

		procedure RequeueOrders is
			tmp: Order;
			DeletedOrders: Integer := 0;
			begin
				for i in 1 .. OrdersCount loop
					if Orders(i).Pending = False then
						DeletedOrders := DeletedOrders + 1;
					end if;
				end loop;
				for i in 1 .. OrdersCount - 1 loop
					if Orders(i).Pending = False then
						for j in i .. OrdersCount - 1 loop
							tmp := Orders(j);
							Orders(j) := Orders(j + 1);
							Orders(j + 1) := tmp;
						end loop;
					end if;
				end loop;
				OrdersCount := OrdersCount - DeletedOrders;
			end RequeueOrders;

		procedure DeliverOrders is
			SerialNumber: Integer;
			NeedToRequeue: Boolean := False;
			begin
				if OrdersCount > 0 then
					for i in 1 .. OrdersCount loop
						if CanDeliver(Orders(i).ModelID) then
							Put_Line("SKLEP: Wyslano model " & ModelName(Orders(i).ModelID) & " S/N"
							 & Integer'Image(ModelSerialNumber(Orders(i).ModelID)) & " do " & CustomerName(Orders(i).CustomerID));
							for W in PartType loop
								Magazyn(W) := Magazyn(W) - PartsRequiredForModel(Orders(i).ModelID, W);
								OnStore := OnStore - PartsRequiredForModel(Orders(i).ModelID, W);
							end loop;
							SerialNumber := ModelSerialNumber(Orders(i).ModelID);
							K(Orders(i).CustomerID).ReciveOrderedModel(Orders(i).ModelID, SerialNumber);
							ModelSerialNumber(Orders(i).ModelID) := ModelSerialNumber(Orders(i).ModelID) + 1;
							Orders(i).Pending := False;
							NeedToRequeue := True;
						end if;
					end loop;
					if NeedToRequeue then
						RequeueOrders;
					end if;
				end if;
			end DeliverOrders;		

		begin
			Put_Line("SKLEP: Wielkie otwarcie! AndroidShop rozpoczyna dzialalnosc!");
			loop
				select
					when OrdersCount < MaxOrders => accept AcceptOrder(CustomerID: in CustomerType; OrderedModel: in ModelType) do
						OrdersCount := OrdersCount + 1;
						Orders(OrdersCount) := (CustomerID, OrderedModel, True);
						Put_Line("SKLEP: Przyjeto zamowienie od " & CustomerName(CustomerID) & " na model: " & ModelName(OrderedModel));
						PrintOrders;
					end AcceptOrder;
				or
					when OnStore < StoreCapacity => accept ReciveParts(Part: in PartType; SerialNumber: in Integer; Recived: out Boolean) do
						Recived := CanRecive(Part);
						if Recived then
							Put_Line("SKLEP: Przyjelismy dostawe " & PartName(Part) & " S/N" & Integer'Image(SerialNumber));
							Magazyn(Part) := Magazyn(Part) + 1;
							OnStore := OnStore + 1;
						else
							Put_Line("Nie przyjelismy dostawy " & PartName(Part) & " S/N" & Integer'Image(SerialNumber));
						end if;
					end ReciveParts;
				else
					DeliverOrders;	
				end select;	
			end loop;
		end AndroidShop;

	begin
		delay 1.0;
		for I in 1 .. DifferentPartsNumber loop
			P(I).Start(I);
			delay 0.5;
		end loop;
	
		for J in 1 .. DifferentCustomersNumber loop
			K(J).Start(J);
			delay 0.5;
		end loop;

	end Symulacja;