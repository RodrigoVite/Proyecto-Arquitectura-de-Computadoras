library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity practica is port(
	Reset, izq, der, enter: in std_logic;
	DB: out std_logic_vector(7 downto 0);
	RS, RW, E: out std_logic;
	DISPLAY: out std_logic_vector(7 downto 0);
	flags: out std_logic_vector(3 downto 0);     -- N V Z C
--	comp: out std_logic_vector(2 downto 0);      -- < = >
	vidas: out std_logic_vector(2 downto 0);
	AN: inout  std_logic_vector(3 downto 0)
	);
end practica;

architecture programa of practica is

component OSCH
	Generic(NOM_FREQ: string:="44.33");--frecuencia dada
	Port(STDBY: in std_logic;
	OSC: out std_logic;
	SEDSTDBY:out std_logic);
end component;

--	function mult5(X: signed; Y: signed) return std_logic_vector is
--		variable m1,m2,m3,m4,m5,m6,m7,m8,auxM: std_logic_vector(15 downto 0) := (OTHERS => '0');
--		variable Aa, Ba: std_logic_vector(7 downto 0);
--	begin
--		if(X(7)='1') then
--			Aa:=std_logic_vector((not X(7 downto 0))+1);
--		else
--			Aa:=std_logic_vector(X(7 downto 0));
--		end if;
--		if(Y(7)='1') then
--			Ba:=std_logic_vector((not Y(7 downto 0))+1);
--		else
--			Ba:=std_logic_vector(Y(7 downto 0));
--		end if;
--		m1(7 downto 0) := ((Aa(7) AND Ba(0)) & (Aa(6) AND Ba(0)) & (Aa(5) AND Ba(0)) & (Aa(4) AND Ba(0)) & (Aa(3) AND Ba(0)) & (Aa(2) AND Ba(0)) & (Aa(1) AND Ba(0)) & (Aa(0) AND Ba(0)));
--		m2(8 downto 1) := ((Aa(7) AND Ba(1)) & (Aa(6) AND Ba(1)) & (Aa(5) AND Ba(1)) & (Aa(4) AND Ba(1)) & (Aa(3) AND Ba(1)) & (Aa(2) AND Ba(1)) & (Aa(1) AND Ba(1)) & (Aa(0) AND Ba(1)));
--		m3(9 downto 2) := ((Aa(7) AND Ba(2)) & (Aa(6) AND Ba(2)) & (Aa(5) AND Ba(2)) & (Aa(4) AND Ba(2)) & (Aa(3) AND Ba(2)) & (Aa(2) AND Ba(2)) & (Aa(1) AND Ba(2)) & (Aa(0) AND Ba(2)));
--		m4(10 downto 3) := ((Aa(7) AND Ba(3)) & (Aa(6) AND Ba(3)) & (Aa(5) AND Ba(3)) & (Aa(4) AND Ba(3)) & (Aa(3) AND Ba(3)) & (Aa(2) AND Ba(3)) & (Aa(1) AND Ba(3)) & (Aa(0) AND Ba(3)));
--		m5(11 downto 4) := ((Aa(7) AND Ba(4)) & (Aa(6) AND Ba(4)) & (Aa(5) AND Ba(4)) & (Aa(4) AND Ba(4)) & (Aa(3) AND Ba(4)) & (Aa(2) AND Ba(4)) & (Aa(1) AND Ba(4)) & (Aa(0) AND Ba(4)));
--		m6(12 downto 5) := ((Aa(7) AND Ba(5)) & (Aa(6) AND Ba(5)) & (Aa(5) AND Ba(5)) & (Aa(4) AND Ba(5)) & (Aa(3) AND Ba(5)) & (Aa(2) AND Ba(5)) & (Aa(1) AND Ba(5)) & (Aa(0) AND Ba(5)));
--		m7(13 downto 6) := ((Aa(7) AND Ba(6)) & (Aa(6) AND Ba(6)) & (Aa(5) AND Ba(6)) & (Aa(4) AND Ba(6)) & (Aa(3) AND Ba(6)) & (Aa(2) AND Ba(6)) & (Aa(1) AND Ba(6)) & (Aa(0) AND Ba(6)));
--		m8(14 downto 7) := ((Aa(7) AND Ba(7)) & (Aa(6) AND Ba(7)) & (Aa(5) AND Ba(7)) & (Aa(4) AND Ba(7)) & (Aa(3) AND Ba(7)) & (Aa(2) AND Ba(7)) & (Aa(1) AND Ba(7)) & (Aa(0) AND Ba(7)));
--		auxM := m8 + m7 + m6 + m5 + m4 + m3 + m1 + m2;
---		if(X(7)='1' and Y(7)='1') then
--			return auxM;
--		elsif(X(7)='0' and Y(7)='0') then
--			return auxM;
--		else
--			return ((not auxM)+1);
--		end if;		
--	end mult5;
	
--	function div5(X: signed; Y: signed) return std_logic_vector is
--	variable c,r,d: unsigned(7 downto 0 );
--	variable auxD: std_logic_vector(15 downto 0);
--	variable Aa, Ba: std_logic_vector(7 downto 0);
--	begin
--		if(X(7)='1') then
--			Aa:=std_logic_vector((not X(7 downto 0))+1);
--		else
--			Aa:=std_logic_vector(X(7 downto 0));
--		end if;
--		if(Y(7)='1') then
--			Ba:=std_logic_vector((not Y(7 downto 0))+1);
--		else
--			Ba:=std_logic_vector(Y(7 downto 0));
--		end if;
--		if Ba(7 downto 0) /= "00000000" then        -- se valida que no sea entre '0'
--			if Ba(7 downto 0) ="00000001" then      -- caso de la division entre 1
--				auxD := std_logic_vector("00000000"&Aa(7 downto 0));
--			else                                    -- caso de la division entre 2 o mas
--				c := (OTHERS => '0');
--				r := unsigned(Aa(7 downto 0));      -- inicializacion del residuo al AUX
--				d := unsigned(Ba(7 downto 0));      -- inicializacion del divisor al AUX2
--				for i in 0 to 128 loop
--					if (r >= d) then                -- mientras el residuo sea mayor o igual al divisor 
--						c := c+1;
--						r := r-d;                   -- se hace una resta
--					end if;
--				end loop;	
--				auxD := "00000000"&std_logic_vector(c);
--			end if;
--		end if;
--		if(X(7)='1' and Y(7)='1') then
--			return auxD;
--		elsif(X(7)='0' and Y(7)='0') then
--			return auxD;
--		else
--			return ((not auxD)+1);
--		end if;
--	end div5;

type ROM is array (200 downto 0) of std_logic_vector(7 downto 0);
type ram_type is array (31 downto 0) of std_logic_vector(15 downto 0);
signal RAM: ram_type;
constant ROM_Program: ROM :=(
		--- Inicio
		0   => "00100000", --
		1   => "00100000", --
		2   => "00100000", --
		3   => "00100000", --
		4	=> "01000001", -- A
		5 	=> "01001000", -- H
		6	=> "01001111", -- O
		7	=> "01010010", -- R
		8	=> "01000011", -- C
		9  	=> "01000001", -- A
		10  => "01000100", -- D
		11  => "01001111", -- O
		12	=> "00100000", --
		13	=> "00100000", --
		14 	=> "00100000", --
		15  => "00100000", --
		16 	=> "01010011", -- S
		17	=> "01100101", -- e
		18 	=> "01101100", -- l
		19	=> "00101110", -- .
		20	=> "00100000", --
		21	=> "01010000", -- P
		22 	=> "01100001", -- a
		23	=> "01101100", -- l
		24 	=> "00101110", -- .
		25	=> "00111010", -- :
		26	=> "00100000", -- 
		27	=> "00100000", -- a (hace referencia al lugar donde se mostrará las opciones)
		28	=> "00100000", --
		29	=> "00100000", --
		30	=> "00100000", --
		31	=> "00100000", --
		--- Palabra 1: h e l p
		32  => "01101000", -- h
		33  => "01011111", -- _
		34  => "01101100", -- l
		35  => "01011111", -- _
		36  => "00100000", --
		37  => "00100000", --
		38  => "00100000", --
		39  => "00100000", --
		40  => "00100000", --
		41  => "00100000", --
		42  => "00100000", --
		43  => "00100000", --
		44  => "00100000", --
		45  => "00100000", --
		46  => "00100000", --
		47  => "00100000", --
		--- Palabra 2: l o b o
		48  => "01011111", -- _
		49  => "01101111", -- o
		50  => "01011111", -- _
		51  => "01101111", -- o
		52  => "00100000", --
		53  => "00100000", --
		54  => "00100000", --
		55  => "00100000", --
		56  => "00100000", --
		57  => "00100000", --
		58  => "00100000", --
		59  => "00100000", --
		60  => "00100000", --
		61  => "00100000", --
		62  => "00100000", --
		63  => "00100000", --
		--- Palabra 3: j u e g o
		64  => "01101010", -- j
		65  => "01011111", -- _
		66  => "01100101", -- e
		67  => "01011111", -- _
		68  => "01101111", -- o
		69  => "00100000", --
		70  => "00100000", --
		71  => "00100000", --
		72  => "00100000", --
		73  => "00100000", --
		74  => "00100000", --
		75  => "00100000", --
		76  => "00100000", --
		77  => "00100000", --
		78  => "00100000", --
		79  => "00100000", --
		--- Palabra 4: m a r i n o
		80  => "01101101", -- m
		81  => "01011111", -- _
		82  => "01110010", -- r
		83  => "01101001", -- i
		84  => "01011111", -- _
		85  => "01101111", -- o
		86  => "00100000", --
		87  => "00100000", --
		88  => "00100000", --
		89  => "00100000", --
		90  => "00100000", --
		91  => "00100000", --
		92  => "00100000", --
		93  => "00100000", --
		94  => "00100000", --
		95  => "00100000", --
		--- Palabra 5: v e n t i l a d o r
		96  => "01110110", -- v
		97  => "01011111", -- _
		98  => "01101110", -- n
		99  => "01110100", -- t
		100  => "01011111", -- _
		101  => "01101100", -- l
		102  => "01011111", -- _
		103  => "01100100", -- d
		104  => "01011111", -- _
		105  => "01110010", -- r
		106  => "00100000", --
		107  => "00100000", --
		108  => "00100000", --
		109  => "00100000", --
		110  => "00100000", --
		111  => "00100000", --
		-- Inicializacion de variables 
		-- Inicio: inicia en 0 
		112  => "00001011", -- Carga el indice del comienzo de la frase (j)
		113  => "00000001",
		114  => "00000000", -- 0 (j)
		115  => "00001111", -- JUMP a la segunda parte de la función
		116  => "10001110", -- 142
		-- Palabra 1: inicia en 32
		117  => "00001011", -- Carga el indice del comienzo de la palabra (j)
		118  => "00000001",
		119  => "00100000", -- 32 (j)
		120  => "00001111", -- JUMP a la segunda parte de la función
		121  => "10011001", -- 153
		-- Palabra 2: inicia en 48
		122  => "00001011", -- Carga el indice del comienzo de la palabra (j)
		123  => "00000001",
		124  => "00110000", -- 48 (j)
		125  => "00001111", -- JUMP a la segunda parte de la función
		126  => "10011001", -- 153
		-- Palabra 3: inicia en 64
		127  => "00001011", -- Carga el indice del comienzo de la palabra (j)
		128  => "00000001",
		129  => "01000000", -- 64 (j)
		130  => "00001111", -- JUMP a la segunda parte de la función
		131  => "10011001", -- 153
		-- Palabra 4: inicia en 80
		132  => "00001011", -- Carga el indice del comienzo de la palabra (j)
		133  => "00000001",
		134  => "01010000", -- 80 (j)
		135  => "00001111", -- JUMP a la segunda parte de la función
		136  => "10011001", -- 153
		-- Palabra 5: inicia en 96
		137  => "00001011", -- Carga el indice del comienzo de la palabra (j)
		138  => "00000001",
		139  => "01100000", -- 96 (j)
		140  => "00001111", -- JUMP a la segunda parte de la función
		141  => "10011001", -- 153
		--- Segunda Parte: Comienza a cargar la frase a la ram
		--- Inicializacion de variables
		142  => "00001011", -- i = 0
		143  => "00000000", -- RegsABCD(0)
		144  => "00000000", -- 0
		145  => "00001011", -- step = 1
		146  => "00000010", -- RegsABCD(2)
		147  => "00000001", -- 1
		148  => "00001011", -- size= 32
		149  => "00000011", -- RegsABCD(3)
		150  => "00100000", -- 32
		151  => "00001111", -- JUMP a la tercera parte
		152  => "10100010", -- 162
		--- Segunda Parte: Comienza a cargar la palabra a la ram
		--- Inicializacion de variables
		153  => "00001011", -- i = 0
		154  => "00000000", -- RegsABCD(0)
		155  => "00000000", -- 0
		156  => "00001011", -- step = 1
		157  => "00000010", -- RegsABCD(2)
		158  => "00000001", -- 1
		159  => "00001011", -- size= 16
		160  => "00000011", -- RegsABCD(3)
		161  => "00010000", -- 16 (No necesita saltar a la tercera parte porque están adyacentes)
		--- Tercera Parte: Ciclo para cargar la palabra o frase a la RAM desde la ROM 
		162  => "00010100", -- ROM(j) to RAM(i) 
		163  => "00000000", -- dest ram i(RegsABCD(0))
		164  => "00000001", -- orig rom j(RegsABCD(1)) 
		165  => "00000111", -- Suma
		166  => "00000010", -- i + step
		167  => "00001110", -- Guarda el MBR en RegsABCD(0)(A)
		168  => "00000000", -- i = i+1-- 
		169  => "00000111", -- Suma
		170  => "00010010", -- j + step
		171  => "00001110", -- Guarda el MBR en RegsABCD(1)(B)
		172  => "00000001", -- j = i+1
		173  => "00001000", -- Resta
		174  => "00000011", -- i-size
		175  => "00010000", -- Brach if i - size != 0 
		176  => "00001001",
		177  => "10100010", -- Goto 162
		others => ("11111111")
);
-----------------------------------Señales para control del Display
signal cuenta: std_logic_vector(15 downto 0);                    -- almacena el dato a multiplexar
signal UNI,DEC,CEN,MIL: std_logic_vector (3 DOWNTO 0);           -- digitos unidades, decenas, centenas y unidad de millar
signal D: std_logic_vector (3 downto 0);                         -- sirve para almacenar los valores del display
signal P: std_logic_vector (15 DOWNTO 0);                        -- asigna UNI, DEC,CEN, MIL
signal selector: std_logic_vector (1 downto 0):= "00";           -- selector de barrido
----------------------------------Señales auxiliares para el ciclo fetch
signal bandera: std_logic_vector (3 downto 0) := (OTHERS => '0');
--signal comparador: std_logic_vector (2 downto 0) := (OTHERS => '0');
signal PC1,PC2: integer;
------------------------------------Registros de proposito especifico
signal dispmode: std_logic;
signal PC : integer := 0; 
signal MAR, IR : std_logic_vector(7 downto 0):="00000000";
signal MBR, ACC : std_logic_vector(15 downto 0):="0000000000000000";
----------------------------------------Registros de entrada a la ALU
type REGISTROS is array (15 downto 0) of std_logic_vector(15 downto 0);
type ESTADOS is (Fetch, Decode, Execute);
signal estado: ESTADOS;
signal RegsABCD: REGISTROS;
signal REAUX, REAUX2, REAUX3 : signed(15 downto 0):="0000000000000000"; -- Auxiliares
------------------------------------Señales para controlar la LCD
TYPE CONTROL IS(power_up, initialize, RESETLINE, line1, line2, send);
type CASO is (frase, palabra);
type GAME is (ini, veri, win, lose);
signal state : CONTROL;
signal est: CASO;
signal juego: GAME;
signal bcdSig : std_logic_vector(11 downto 0);
CONSTANT freq : INTEGER := 133; --system clock frequency in MHz
signal ptr : natural range 0 to 16 := 15; -- To keep track of what character we are up to
signal line	: STD_LOGIC := '1';
signal line1Sig: std_logic_vector(127 DOWNTO 0); -- Guarda lo que se muestra en la linea1 de la LCD
signal line2Sig: std_logic_vector(127 DOWNTO 0); -- Guarda lo que se muestra en la linea2 de la LCD
signal contaux,contaux2: integer := 97;
signal mov_pc: std_logic := '0'; -- Señal auxiliar para poder controlar el PC desde el juego
signal auxV: std_logic_vector(2 downto 0); -- Guarda las vidas que tiene el jugador
signal auxL: std_logic_vector(7 downto 0) := "11111111"; -- Guarda la letra seleccionada por el usuario
signal auxP1,auxP2: std_logic_vector(79 downto 0); -- Señales para la veriicacion de la palabra
-----------------------------------------------------Senales de reloj 
signal CLK: std_logic;

constant max_count_lett2: INTEGER := 5319600;            --numero maximo para la cuenta
signal count_lett2: INTEGER range 0 to max_count_lett2;  --llevara la cuenta hasta el 4433000
signal clk_lett2: std_logic:= '0';                       --senal para el clock a 120 ms

constant max_count_lett: INTEGER := 692656;              --numero maximo para la cuenta
--constant max_count_lett: INTEGER := 11082500;            --numero maximo para la cuenta
signal count_lett: INTEGER range 0 to max_count_lett;    --llevara la cuenta hasta el 692656 || 11082500
signal clk_lett: std_logic:= '0';                        --senal para el clock de las letras de la LCD

constant max_count_med: INTEGER := 10000;                --numero maximo para la cuenta
signal count_med: INTEGER range 0 to max_count_med;      --llevara la cuenta hasta el 10000
signal clk_med: std_logic:= '0';                         --senal para el clock medio

procedure veriBandera(a,x,y: in std_logic_vector(15 downto 0); signal bandera: out std_logic_vector(3 downto 0)) is
begin
	if(a(14)='1') then                                     --Negative
		bandera(3)<='1';
	else 
		bandera(3)<='0';
	end if;
	
	bandera(2)<= a(14) xor x(14) xor y(14) xor a(15);      --Overflow
	
	if(a(14 downto 0)="000000000000000") then              --Zero
		bandera(1)<='1';
	else 
		bandera(1)<='0';
	end if;
	if(a(15)='1') then                                     --Carry
		bandera(0)<='1';
	else 
		bandera(0)<='0';
	end if;
end veriBandera;

begin
	
	OSCInst0: OSCH
	GENERIC MAP(NOM_FREQ =>"44.33")
	PORT MAP(STDBY =>'0',OSC=>CLK,SEDSTDBY=>OPEN);
	
	gen_clk_medio: process(CLK) --reduccion del clock de 44.33 MHz
	begin
		if(CLK'event and CLK='1') then
			if (count_med<max_count_med) then
				count_med <= count_med+1;
			else
				clk_med <= not clk_med;
				count_med <= 0;
			end if;
		end if;
	end process gen_clk_medio;
	
	gen_clk_lett: process(CLK) --reduccion del clock de 44.33 MHz
	begin
		if(CLK'event and CLK='1') then
			if (count_lett<max_count_lett) then
				count_lett <= count_lett+1;
			else
				clk_lett <= not clk_lett;
				count_lett <= 0;
			end if;
		end if;
	end process gen_clk_lett;
	
	gen_clk_lett2: process(CLK) --reduccion del clock de 44.33 MHz a 120 ms
	begin
		if(CLK'event and CLK='1') then
			if (count_lett2<max_count_lett2) then
				count_lett2 <= count_lett2+1;
			else
				clk_lett2 <= not clk_lett2;
				count_lett2 <= 0;
			end if;
		end if;
	end process gen_clk_lett2;
	 
	dispmode<= 		RegsABCD(15)(0);

	ControlUnit: process(Reset, PC, IR, CLK_lett, mov_pc )
	begin  
		PC1 <= PC+1;
		PC2 <= PC+2;
		if (Reset = '0') then 
			estado <= Fetch;
			flags <= "0000";
--			comp <= "000";
			IR <= (others =>'0');
			REAUX <= (others =>'0');
			REAUX2 <= (others =>'0');
			MBR <= (others =>'0');
			RegsABCD <= (others => "0000000000000000");
			PC <= 112;
		elsif (CLK_lett'event and CLK_lett = '1')then
--		elsif (CLK'event and CLK = '1' and IR /="11111111")then
			if (mov_pc = '1') then
				case (contaux2) is
					when 97  =>
						PC <= 117;
						estado <= Fetch;
					when 98  =>
						PC <= 122;
						estado <= Fetch;
					when 99  =>
						PC <= 127;
						estado <= Fetch;
					when 100 =>
						PC <= 132;
						estado <= Fetch;
					when others =>
						PC <= 137;
						estado <= Fetch;
				end case;
			end if;
			case estado is
				when Fetch =>
					IR <= ROM_program(PC);
					MAR <= ROM_program(PC1);
					estado <= Decode;
				when Decode =>
					if (IR = "11111111") then
						PC <= PC;
						estado <= Fetch;
					elsif (IR = "00001011") then                 -- Cargar Num a RegsABCD
						RegsABCD(to_integer(unsigned(MAR))) <= "00000000"&ROM_program(PC2);
						PC <= PC+3;
						estado <= Fetch;
					elsif (IR = "00001100") then                 -- Cargar Dato de ROM_prog a RegsABCD
						RegsABCD(to_integer(unsigned(MAR(7 downto 4)))) <=  "00000000"&ROM_program(to_integer(unsigned(MAR(1 downto 0))));
						estado <= Fetch;
						PC <= PC+2;
					elsif (IR = "00001101") then                 -- Copiar Registro(orig) a Registro(dest)
						RegsABCD(to_integer(unsigned(MAR(7 downto 4))))<= RegsABCD(to_integer(unsigned(MAR(3 downto 0))));
						estado <= Fetch;
						PC <= PC+2;
					elsif (IR = "00001110") then                 -- Copiar del MBR al Registro(dest)
						RegsABCD(to_integer(unsigned(MAR)))<=  MBR;
						estado <= Fetch;
						PC <= PC+2;
					elsif (IR = "00001111") then                  -- Jump a una direccion
						estado <= Fetch;
						PC <= to_integer(unsigned(MAR));
					elsif (IR = "00010000") then                  -- Branch
						if((MAR(3 downto 0) xor bandera)="0000") then 
							PC <= to_integer(unsigned(ROM_program(PC2)));
						else  
							PC <= PC+3; 
						end if;		 	
						estado <= Fetch;
					elsif (IR = "00010001") then                  -- Comparador
						
					elsif (IR = "00010010") then                  -- Escribir del MBR a la RAM
						RAM(to_integer(unsigned(MAR))) <= MBR;
						PC <= PC+2;
						estado <= Fetch;
					elsif (IR = "00010011") then                  -- Leer de la RAM a RegsABCD
						RegsABCD(to_integer(unsigned(MAR))) <= RAM(to_integer(unsigned(RegsABCD(to_integer(unsigned(ROM_Program(PC2)))))));
						PC <= PC+3;
						estado <= Fetch;
					elsif (IR = "00010100") then                  -- Escribir de la ROM a la RAM
						RAM(to_integer(unsigned(RegsABCD(to_integer(unsigned(MAR)))))) <= "00000000"&ROM_program(to_integer(unsigned(RegsABCD(to_integer(unsigned(ROM_Program(PC2)))))));
						PC <= PC+3;
						estado <= Fetch;	
					elsif (IR = "00010101") then                  -- Omitir el resto del ciclo fetch
						
					elsif (IR = "00010110") then
						
					else
						REAUX <= signed(RegsABCD(to_integer(unsigned(MAR(7 downto 4)))));
						REAUX2 <= signed(RegsABCD(to_integer(unsigned(MAR(3 downto 0)))));
						estado <= Execute;
					end if;
				when Execute =>
					MBR <= ACC;
					flags <= bandera;
--					comp <= comparador;
					estado <= Fetch;    
					PC <= PC + 2;
			end case;
		end if; 
		cuenta <= '0'&MBR(14 downto 0);
	end process;
	
	ahor: process( clk_lett2, izq, der, enter, line1Sig, line2Sig, mov_pc, contaux2)  -- Control del ahorcado
	begin
		if(clk_lett2'EVENT and clk_lett2 = '1') then
			if (reset = '0') then
				est <= frase;
				auxV <= "111";
				auxL <= "11111111";
				contaux <= 97;
				contaux2 <= 97;
			end if;
			case est is
				when frase =>
					line1Sig <= 	RAM(0)(7 DOWNTO 0)& 
									RAM(1)(7 DOWNTO 0)&
									RAM(2)(7 DOWNTO 0)&
									RAM(3)(7 DOWNTO 0)&
									RAM(4)(7 DOWNTO 0)&
									RAM(5)(7 DOWNTO 0)&
									RAM(6)(7 DOWNTO 0)&
									RAM(7)(7 DOWNTO 0)&
									RAM(8)(7 DOWNTO 0)&
									RAM(9)(7 DOWNTO 0)&
									RAM(10)(7 DOWNTO 0)&
									RAM(11)(7 DOWNTO 0)&
									RAM(12)(7 DOWNTO 0)&
									RAM(13)(7 DOWNTO 0)&
									RAM(14)(7 DOWNTO 0)&
									RAM(15)(7 DOWNTO 0);
					line2Sig <= 	RAM(16)(7 DOWNTO 0)&
									RAM(17)(7 DOWNTO 0)&
									RAM(18)(7 DOWNTO 0)&
									RAM(19)(7 DOWNTO 0)&
									RAM(20)(7 DOWNTO 0)&
									RAM(21)(7 DOWNTO 0)&
									RAM(22)(7 DOWNTO 0)&
									RAM(23)(7 DOWNTO 0)&
									RAM(24)(7 DOWNTO 0)&
									RAM(25)(7 DOWNTO 0)&
									RAM(26)(7 DOWNTO 0)&
									std_logic_vector(to_unsigned(contaux, IR'length))&
									RAM(28)(7 DOWNTO 0)&
									RAM(29)(7 DOWNTO 0)&
									RAM(30)(7 DOWNTO 0)&
									RAM(31)(7 DOWNTO 0);
					if(izq='1') then
						if(contaux=97) then
							contaux <= 101;
						else
							contaux <= contaux - 1;
						end if;
					elsif(der='1') then 
						if(contaux=101) then
							contaux <= 97;
						else
							contaux <= contaux + 1;
						end if;
					elsif(enter='1') then
						est <= palabra;
						juego <= ini;
						line2Sig <= "00100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000"; 
						contaux2 <= contaux;
						contaux <= 97;
						mov_pc <= '1';
					end if;
				when palabra =>
					mov_pc <= '0';
					vidas <= auxV;
					auxP2 <= line1Sig(127 downto 48);
					case juego is
						when ini =>
							line1Sig <= 	RAM(0)(7 DOWNTO 0)& 
											RAM(1)(7 DOWNTO 0)&
											RAM(2)(7 DOWNTO 0)&
											RAM(3)(7 DOWNTO 0)&
											RAM(4)(7 DOWNTO 0)&
											RAM(5)(7 DOWNTO 0)&
											RAM(6)(7 DOWNTO 0)&
											RAM(7)(7 DOWNTO 0)&
											RAM(8)(7 DOWNTO 0)&
											RAM(9)(7 DOWNTO 0)&
											RAM(10)(7 DOWNTO 0)&
											RAM(11)(7 DOWNTO 0)&
											RAM(12)(7 DOWNTO 0)&
											RAM(13)(7 DOWNTO 0)&
											RAM(14)(7 DOWNTO 0)&
											RAM(15)(7 DOWNTO 0);
							line2Sig(119 downto 112) <= std_logic_vector(to_unsigned(contaux, IR'length));
						when veri =>
							case (contaux2) is
								when 97  =>
									if(auxV /= "000") then 
										if(auxP2 = "01101000011001010110110001110000001000000010000000100000001000000010000000100000") then -- help
											juego <= win;
										else
											if(auxL = "01100101") then     -- e
												auxP1(71 downto 64) <= "01100101";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "01110000") then  -- p
												auxP1(55 downto 48) <= "01110000";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "11111111") then
												auxP1 <= auxP1;
											else
												auxL <= "11111111";
												auxP1 <= auxP1;
												auxV <= to_stdlogicvector(to_bitvector(auxV) srl 1);
												contaux <= 97;
											end if;
											line1Sig(127 downto 48) <= auxP1;
										end if;
									else
										juego <= lose;
									end if;
								when 98  =>
									if(auxV /= "000") then 
										if(auxP2 = "01101100011011110110001001101111001000000010000000100000001000000010000000100000") then -- lobo
											line1Sig <= line1Sig;
											juego <= win;
										else
											if(auxL = "01101100") then     -- l
												auxP1(79 downto 72) <= "01101100";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "01100010") then  -- b
												auxP1(63 downto 56) <= "01100010";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "11111111") then
												auxP1 <= auxP1;
											else
												auxL <= "11111111";
												auxP1 <= auxP1;
												auxV <= to_stdlogicvector(to_bitvector(auxV) srl 1);
												contaux <= 97;
											end if;
											line1Sig(127 downto 48) <= auxP1;
										end if;
									else
										juego <= lose;
									end if;
								when 99  =>
									if(auxV /= "000") then 
										if(auxP2 = "01101010011101010110010101100111011011110010000000100000001000000010000000100000") then -- juego
											line1Sig <= line1Sig;
											juego <= win;
										else
											if(auxL = "01110101") then     -- u
												auxP1(71 downto 64) <= "01110101";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "01100111") then  -- g
												auxP1(55 downto 48) <= "01100111";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "11111111") then
												auxP1 <= auxP1;
											else
												auxL <= "11111111";
												auxP1 <= auxP1;
												auxV <= to_stdlogicvector(to_bitvector(auxV) srl 1);
												contaux <= 97;
											end if;
											line1Sig(127 downto 48) <= auxP1;
										end if;
									else
										juego <= lose;
									end if;
								when 100 =>
									if(auxV /= "000") then 
										if(auxP2 = "01101101011000010111001001101001011011100110111100100000001000000010000000100000") then -- marino
											line1Sig <= line1Sig;
											juego <= win;
										else
											if(auxL = "01100001") then     -- a
												auxP1(71 downto 64) <= "01100001";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "01101110") then  -- n
												auxP1(47 downto 40) <= "01101110";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "11111111") then
												auxP1 <= auxP1;
											else
												auxL <= "11111111";
												auxP1 <= auxP1;
												auxV <= to_stdlogicvector(to_bitvector(auxV) srl 1);
												contaux <= 97;
											end if;
											line1Sig(127 downto 48) <= auxP1;
										end if;
									else
										juego <= lose;
									end if;
								when others =>
									if(auxV /= "000") then 
										if(auxP2 = "01110110011001010110111001110100011010010110110001100001011001000110111101110010") then -- ventilador
											juego <= win;
										else
											if(auxL = "01100101") then     -- e
												auxP1(71 downto 64) <= "01100101";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "01101001") then  -- i
												auxP1(47 downto 40) <= "01101001";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "01100001") then  -- a
												auxP1(31 downto 24) <= "01100001";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "01101111") then  -- o
												auxP1(15 downto 8) <= "01101111";
												auxL <= "11111111";
												contaux <= 97;
											elsif(auxL = "11111111") then
												auxP1 <= auxP1;
											else
												auxL <= "11111111";
												auxP1 <= auxP1;
												auxV <= to_stdlogicvector(to_bitvector(auxV) srl 1);
												contaux <= 97;
											end if;
											line1Sig(127 downto 48) <= auxP1;
										end if;
									else
										juego <= lose;
									end if;
							end case;
							line2Sig(119 downto 112) <= std_logic_vector(to_unsigned(contaux, IR'length));
						when win =>
							line2Sig(119 downto 72) <= "010101010010000001010111010010010100111000100000";
						when lose =>
							line2Sig(119 downto 72) <= "010101010010000001001100010011110101001101000101";
					end case;
					if(izq='1') then
						if(contaux=97) then
							contaux <= 122;
						else
							contaux <= contaux - 1;
						end if;
					elsif(der='1') then 
						if(contaux=122) then
							contaux <= 97;
						else
							contaux <= contaux + 1;
						end if;
					elsif(enter='1') then
						juego <= veri;
						auxL <= std_logic_vector(to_unsigned(contaux, IR'length));
						auxP1 <= line1Sig(127 downto 48);
					end if;
			end case;
		end if;
	end process ahor;

	regALU : process(IR, REAUX, REAUX2) -- ALU
	variable shift : std_logic_vector(15 downto 0);
	variable desplazamientos: integer;
	begin
		case IR is
			when "00000001" => ACC <= std_logic_vector(not REAUX);
			when "00000010" => ACC <= std_logic_vector(REAUX and REAUX2);
			when "00000011" => ACC <= std_logic_vector((not REAUX) + 1);
			when "00000100" => ACC <= std_logic_vector(REAUX or REAUX2);
--			when "00000101" | "00000110" =>
--				shift := std_logic_vector(REAUX);
--				desplazamientos := to_integer(REAUX2);
--				for i in 0 to (10) loop
--					if (desplazamientos > 0)then 
--						if(IR = "00000101") then                         --LSL
--							shift:= shift(14 downto 0) & "0";
--						else                                             --ASR
--							shift:= shift(15) & shift(15 downto 1);
--						end if;
--						desplazamientos := desplazamientos - 1;  
--					end if; 			
--				end loop; 
--				ACC <= shift;
			when "00000111" => ACC <= std_logic_vector(REAUX+REAUX2);       --S U M A
				veriBandera(ACC,std_logic_vector(REAUX),std_logic_vector(REAUX2),bandera);
			when "00001000" =>-- REAUX3 <= REAUX-REAUX2;                        --R E S T A
				REAUX3(14 downto 0)<= (not REAUX2(14 downto 0)) + 1;     --complemento a 2
				if((REAUX(14) xor REAUX3(14))='1') then
					ACC <= std_logic_vector(REAUX-REAUX2);
					veriBandera(ACC,std_logic_vector(REAUX),std_logic_vector(REAUX2),bandera);
				else
					ACC <= std_logic_vector(((not REAUX)+1)+REAUX2);
					REAUX3 <= REAUX+((not REAUX2)+1);
					veriBandera(std_logic_vector(REAUX3),std_logic_vector(REAUX),std_logic_vector((not REAUX2)+1),bandera);
				end if;
--			when "00001001" => REAUX3 <= signed(mult5(REAUX,REAUX2));               --M U L T I P L I C A C I O N
--					veriBandera(std_logic_vector(REAUX3),std_logic_vector(REAUX),std_logic_vector(REAUX2),bandera);
--					if(bandera(3)='1') then
--						ACC <= std_logic_vector((not REAUX3)+1);
--					else
--						ACC <= std_logic_vector(REAUX3);
--					end if;
--			when "00001010" => REAUX3 <= signed(div5(REAUX,REAUX2));                --D I V I S I O N
--					veriBandera(std_logic_vector(REAUX3),std_logic_vector(REAUX),std_logic_vector(REAUX2),bandera);
--					if(bandera(3)='1') then
--						ACC <= std_logic_vector((not REAUX3)+1);
--					else
--						ACC <= std_logic_vector(REAUX3);
--					end if;
			when others => ACC <= (OTHERS => '0');	
		end case;
	end process;
	
	asignacion:PROCESS(cuenta) --Algoritmo Shift and Add 3
	VARIABLE UM_C_D_U:STD_LOGIC_VECTOR(29 DOWNTO 0);   --26 bits para separar las U.Millar-Centenas-Decenas-Unidades
	BEGIN
		--ciclo de inicialización
		FOR I IN 0 TO 29 LOOP
			UM_C_D_U(I):='0'; -- se inicializa con 0
		END LOOP;
		UM_C_D_U(13 DOWNTO 0):=cuenta(13 DOWNTO 0); --contador de 14 bits

		--ciclo de asignación UM-C-D-U
		FOR I IN 0 TO 13 LOOP   -- FOR I IN 0 TO 13 LOOP -- si carga desde shift4 solo hace 10 veces el ciclo shift and add
		-- los siguientes condicionantes comparan (>=5) y suman 3
			IF UM_C_D_U(17 DOWNTO 14) > 4 THEN -- U
				UM_C_D_U(17 DOWNTO 14):= UM_C_D_U(17 DOWNTO 14)+3;
			END IF;
			IF UM_C_D_U(21 DOWNTO 18) > 4 THEN -- D
				UM_C_D_U(21 DOWNTO 18):= UM_C_D_U(21 DOWNTO 18)+3;
			END IF;
			IF UM_C_D_U(25 DOWNTO 22) > 4 THEN -- C
				UM_C_D_U(25 DOWNTO 22):= UM_C_D_U(25 DOWNTO 22)+3;
			END IF;
			IF UM_C_D_U(29 DOWNTO 26) > 4 THEN -- UM
				UM_C_D_U(29 DOWNTO 26):= UM_C_D_U(29 DOWNTO 26)+3;
			END IF;
			UM_C_D_U(29 DOWNTO 1):= UM_C_D_U(28 DOWNTO 0);-- realiza el corrimiento
		END LOOP;
		P<=UM_C_D_U(29 DOWNTO 14);-- guarda en P y en seguida se separan UM-C-D-U
		--UNIDADES
		UNI<=P(3 DOWNTO 0);
		--DECENAS
		DEC<=P(7 DOWNTO 4);
		--CENTENAS
		CEN<=P(11 DOWNTO 8);
		--MILLARES
		MIL<=P(15 DOWNTO 12);
	END PROCESS asignacion;

	PROCESS(clk_med, UNI, DEC, CEN, MIL) -- Multiplexion del Display
	BEGIN
				IF (rising_edge(clk_med)) THEN --Multiplexacion
					selector <= selector+'1';
					CASE(selector) IS
						when "00" => AN <="1000"; D <= UNI; -- UNIDADES
						when "01" => AN <="0100"; D <= DEC; -- DECENAS
						when "10" => AN <="0010"; D <= CEN; -- CENTENAS
						when OTHERS=>AN <="0001"; D <= MIL; -- UNIDAD DE MILLAR
					END CASE;
				end if;
				case(D) is 					-- .abcdefg
					WHEN "0000" => DISPLAY <= "01111110"; --0
					WHEN "0001" => DISPLAY <= "00110000"; --1
					WHEN "0010" => DISPLAY <= "01101101"; --2
					WHEN "0011" => DISPLAY <= "01111001"; --3
					WHEN "0100" => DISPLAY <= "00110011"; --4
					WHEN "0101" => DISPLAY <= "01011011"; --5
					WHEN "0110" => DISPLAY <= "01011111"; --6
					WHEN "0111" => DISPLAY <= "01110000"; --7
					WHEN "1000" => DISPLAY <= "01111111"; --8
					WHEN "1001" => DISPLAY <= "01110011"; --9
					WHEN OTHERS => DISPLAY <= "00000000"; --apagado
				END CASE;
	END PROCESS;
	
	contLCD: process(clk,reset,line1Sig,line2Sig,dispmode) -- Interfaz de la LCD
		variable count: integer := 0;
	begin
		if(Reset = '0') then 
			state <= power_up;
		elsIF(clk'EVENT and clk = '1') THEN    
			CASE state IS
				WHEN power_up =>	--wait 50 ms to ensure Vdd has risen and required LCD wait is met
					IF(count < (50000 * freq)) THEN    --wait 50 ms
						count := count + 1;
						state <= power_up;
					ELSE                                   --power-up complete
						count := 0;
						RS <= '0'; 
						RW <= '0';
						DB <= "00110000";
						state <= initialize;
					END IF;
				WHEN initialize =>	--cycle through initialization sequence  
					count := count + 1;
					IF(count < (10 * freq)) THEN       --function set
						DB <= "00111100";      --2-line mode, display on
						--DB <= "00110100";    --1-line mode, display on
						--DB <= "00110000";    --1-line mdoe, display off
						--DB <= "00111000";    --2-line mode, display off
						E <= '1'; 
						state <= initialize;
					ELSIF(count < (60 * freq)) THEN    --wait 50 us
						DB <= "00000000";
						e <= '0';
						state <= initialize;
					ELSIF(count < (70 * freq)) THEN    --display on/off control
						DB <= "00001100";      --display on, cursor off, blink off
						--DB <= "00001101";    --display on, cursor off, blink on
						--DB <= "00001110";    --display on, cursor on, blink off
						--DB <= "00001111";    --display on, cursor on, blink on
						--DB <= "00001000";    --display off, cursor off, blink off
						--DB <= "00001001";    --display off, cursor off, blink on
						--DB <= "00001010";    --display off, cursor on, blink off
						--DB <= "00001011";    --display off, cursor on, blink on            
						E <= '1';
						state <= initialize;
					ELSIF(count < (120 * freq)) THEN   --wait 50 us
						DB <= "00000000";
						E <= '0';
						state <= initialize;
					ELSIF(count < (130 * freq)) THEN   --display clear
						DB <= "00000001";
						E <= '1';
						state <= initialize;
					ELSIF(count < (2130 * freq)) THEN  --wait 2 ms
						DB <= "00000000";
						E <= '0';
						state <= initialize;
					ELSIF(count < (2140 * freq)) THEN  --entry mode set
						DB <= "00000110";      --increment mode, entire shift off
						--DB <= "00000111";    --increment mode, entire shift on
						--DB <= "00000100";    --decrement mode, entire shift off
						--lcd_data <= "00000101";    --decrement mode, entire shift on
						E <= '1';
						state <= initialize;
					ELSIF(count < (2200 * freq)) THEN  --wait 60 us
						DB <= "00000000";
						E <= '0';
						state <= initialize;
					ELSE                                   --initialization complete
						count := 0;
						state <= RESETLINE;
					END IF;    
				WHEN RESETLINE =>
					ptr <= 16;
					if line = '1' then
						DB <= "10000000";
						RS <= '0';
						RW <= '0';
						count := 0; 
						state <= send;
					else
						DB <= "11000000";
						RS <= '0';
						RW <= '0';
						count := 0; 
						state <= send;
					end if;
				WHEN line1 =>
					line <= '1';
					if dispmode = '1' and (ptr = 6 or ptr =7 )then 
						if ptr = 7 then
							DB <= "0011"&bcdsig(7 downto 4);
						else
							DB <= "0011"&bcdsig(3 downto 0);
						end if;
					else
						DB <= line1Sig(ptr*8 + 7 downto ptr*8);
					end if;
					RS <= '1'; 
					RW <= '0';
					count := 0;  
					line <= '1';
					state <= send;	
				WHEN line2 =>
					line <= '0';
					DB <= line2Sig(ptr*8 + 7 downto ptr*8);
					RS <= '1';
					RW <= '0';
					count := 0;             
					state <= send;
				WHEN send =>	--send instruction to lcd
					IF(count < (50 * freq)) THEN  --do not exit for 50us 
						IF(count < freq) THEN      --negative enable
							E <= '0';
						ELSIF(count < (14 * freq)) THEN  --positive enable half-cycle
							E <= '1';
						ELSIF(count < (27 * freq)) THEN  --negative enable half-cycle
							E <= '0';
						END IF;
						count := count + 1;
						state <= send;
					ELSE
						count := 0;
						if line = '1' then
						   if ptr = 0 then
								line <= '0';
								state <= resetline;
							else
								ptr <= ptr - 1;
								state <= line1;
							end if;
						else
							if ptr = 0 then
								line <= '1';
								state <= resetline;
							else
								ptr <= ptr - 1;
								state <= line2;
							end if;
						end if;
					END IF; 
			END CASE;
		END IF;
	end process 