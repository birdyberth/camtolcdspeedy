---------------------------------
-- Top level Camera to LCD
---------------------------------
-- library declaration
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
-- entity
entity camtolcdspeedy is
	port ( CLOCK_50_IN : in std_logic;
			D5M_PIXCLK_IN : in std_logic;
			D5M_LVAL_IN : in std_logic;
			D5M_FVAL_IN : in std_logic;
			D5M_Di_IN : in std_logic_vector(11 downto 0);
			RESET : in std_logic;
			-------------------------
			D5M_XCLKIN_OUT : out std_logic;
			D5M_SDA_OUT : inout std_logic; 
			D5M_SCL_OUT : inout std_logic;
			LT_DBi_OUT : out std_logic_vector(15 downto 0); -- we use it as OUT only
			LT_WR_n_OUT : out std_logic; -- aka WRX
			LT_RD_n_OUT : out std_logic; -- aka RDX
			LT_RS_OUT : out std_logic; -- aka D/CX
			LT_CS_n_OUT : out std_logic; -- aka CSX
			LT_RESET_n_OUT : out std_logic; -- aka RESX
			LT_LCD_ON_OUT : out std_logic; -- Transistor to drive LCD lighting (1 = on)
			D5M_RESET_n_OUT : out std_logic);
end camtolcdspeedy;
-- architecture
architecture my_camtolcd of camtolcdspeedy is
	-- components
	-- Clock output to the D5M camera
	component D5M_clock is
		port ( CLOCK_50 : in std_logic;
			D5M_XCLKIN : out std_logic);
	end component;
	
	-- i2c master controller from digikey, for setting camera parameters
	component i2c_master is
		generic ( input_clk : integer := 50_000_000; --input clock speed from user logic in Hz
					bus_clk   : integer := 400_000);   --speed the i2c bus (scl) will run at in Hz
		port ( clk       : in     std_logic;                    --system clock
				reset_n   : in     std_logic;                    --active low reset
				ena       : in     std_logic;                    --latch in command
				addr      : in     std_logic_vector(6 downto 0); --address of target slave
				rw        : in     std_logic;                    --'0' is write, '1' is read
				data_wr   : in     std_logic_vector(7 downto 0); --data to write to slave
				------------------------------------------------------------------------------------
				busy      : out    std_logic;                    --indicates transaction in progress
				data_rd   : out    std_logic_vector(7 downto 0); --data read from slave
				ack_error : buffer std_logic;                    --flag if improper acknowledge from slave
				sda       : inout  std_logic;                    --serial data output of i2c bus
				scl       : inout  std_logic);                   --serial clock output of i2c bus
	end component;
	
	-- Camera configuration data feeder to write all registers
	component datafeeder is
		port ( feed_clk : in std_logic; 
				i2c_busy : in std_logic;
				i2c_data_rd : in std_logic_vector(7 downto 0);
				feed_rst_n : in std_logic;
				--------------------------------
				--aclr_out : out std_logic;
				i2c_reset_n : out std_logic;
				i2c_ena : out std_logic;
				i2c_addr : out std_logic_vector(6 downto 0);
				i2c_rw : out std_logic;
				i2c_data_wr : out std_logic_vector(7 downto 0);
				data : out std_logic_vector (15 downto 0));
	end component;
	
	-- Pixel interfacing --------------
	component pixel_interface is
		port ( D5M_PIXCLK : in std_logic;
				D5M_LVAL : in std_logic;
				D5M_FVAL : in std_logic;
				D5M_Di : in std_logic_vector(11 downto 0);
				---------------------------------------------
				pixel_out : out std_logic_vector(4 downto 0);
				Xpos_out : out unsigned(11 downto 0);
				Ypos_out : out unsigned(10 downto 0));
	end component;
	
	-- Bayer fifo
	component bayerfifo
		port( aclr		: in std_logic := '0';
				data		: in std_logic_vector(4 downto 0);
				rdclk		: in std_logic;
				rdreq		: in std_logic;
				wrclk		: in std_logic;
				wrreq		: in std_logic;
				q	: out std_logic_vector(4 downto 0));
	end component;
	
	-- Image transformation ---------
	component image_transform is
		port( D5M_PIXCLK : in std_logic;
				pixel_in : in std_logic_vector(4 downto 0);
				Xpos_in : in unsigned(11 downto 0);
				Ypos_in : in unsigned(10 downto 0);
				q_in : in std_logic_vector(4 downto 0);
				-----------------------------------
				data_out : out std_logic_vector(4 downto 0);
				rdclk_out : out std_logic;
				rdreq_out : out std_logic;
				wrclk_out : out std_logic;
				wrreq_out : out std_logic;
				pixImage_out : out std_logic_vector(15 downto 0);
				newpixclk_out : out std_logic);
	end component;
	
	-- LCD controller --------------------
	component lcd_controller is
		port ( Clk : in std_logic;
				Rst : in std_logic;
				pixImage_in : in std_logic_vector(15 downto 0);
				newpixclk_in : in std_logic;
				--------------------
				LT_DBi : out std_logic_vector(15 downto 0); -- we use it as OUT only
				LT_WR_n : out std_logic; -- aka WRX
				LT_RD_n : out std_logic; -- aka RDX
				LT_RS : out std_logic; -- aka D/CX
				LT_CS_n : out std_logic; -- aka CSX
				LT_RESET_n : out std_logic; -- aka RESX
				LT_LCD_ON : out std_logic; -- Transistor to drive LCD lighting (1 = on)
				D5M_RESET_n : out std_logic;
				setcamconfig : out std_logic);
	end component;
	
	-- Intermediate signal declaration
	signal pixel : std_logic_vector(4 downto 0);
	signal Xpos : unsigned(11 downto 0);
	signal Ypos : unsigned(10 downto 0);
	signal pixImage : std_logic_vector(15 downto 0);
	signal newpixclk : std_logic;
	signal data : std_logic_vector(4 downto 0);
	signal rdclk : std_logic;
	signal rdreq : std_logic;
	signal wrclk : std_logic;
	signal wrreq : std_logic;
	signal q : std_logic_vector(4 downto 0);
	-- signals for i2c
	signal bsy : std_logic;
	signal datread : std_logic_vector(7 downto 0);
	signal rstn : std_logic;
	signal en : std_logic;
	signal ad : std_logic_vector(6 downto 0);
	signal rewr : std_logic;
	signal datwr : std_logic_vector(7 downto 0);
	signal camconfig : std_logic;
	
begin
	clkgen: D5M_clock port map ( CLOCK_50 => CLOCK_50_IN,
											D5M_XCLKIN => D5M_XCLKIN_OUT);
											
	i2cmast: i2c_master port map ( clk => CLOCK_50_IN,
											reset_n => rstn,
											ena => en,
											addr => ad,
											rw => rewr,
											data_wr => datwr,
											busy => bsy,
											data_rd => datread,
											sda => D5M_SDA_OUT,
											scl => D5M_SCL_OUT);
											
	datfeed: datafeeder port map ( feed_clk => CLOCK_50_IN,
											i2c_busy => bsy,
											i2c_data_rd => datread,
											feed_rst_n => camconfig,
											i2c_reset_n => rstn,
											i2c_ena => en,
											i2c_addr => ad,
											i2c_rw => rewr,
											i2c_data_wr => datwr);
	
	pixinter: pixel_interface port map ( D5M_PIXCLK => D5M_PIXCLK_IN,
													D5M_LVAL => D5M_LVAL_IN,
													D5M_FVAL => D5M_FVAL_IN,
													D5M_Di => D5M_Di_IN,
													pixel_out => pixel,
													Xpos_out => Xpos,
													Ypos_out => Ypos);
	
	bayerfif: bayerfifo port map ( data => data,
												rdclk => rdclk,
												rdreq => rdreq,
												wrclk => wrclk,
												wrreq => wrreq,
												q => q);
											
	imgtransform: image_transform port map ( D5M_PIXCLK => D5M_PIXCLK_IN,
															pixel_in => pixel,
															Xpos_in => Xpos,
															Ypos_in => Ypos,
															q_in => q,
															data_out => data,
															rdclk_out => rdclk,
															rdreq_out => rdreq,
															wrclk_out => wrclk,
															wrreq_out => wrreq,
															pixImage_out => pixImage,
															newpixclk_out => newpixclk);
	
	lcdctrl : lcd_controller port map ( Clk => CLOCK_50_IN,
													Rst => RESET,
													pixImage_in => pixImage,
													newpixclk_in => newpixclk,
													LT_DBi => LT_DBi_OUT,
													LT_WR_n => LT_WR_n_OUT,
													LT_RD_n => LT_RD_n_OUT,
													LT_RS => LT_RS_OUT,
													LT_CS_n => LT_CS_n_OUT,
													LT_RESET_n => LT_RESET_n_OUT,
													LT_LCD_ON => LT_LCD_ON_OUT,
													D5M_RESET_n => D5M_RESET_n_OUT,
													setcamconfig => camconfig);
end my_camtolcd;							


---------------------------------
-- Clock output to the D5M camera
---------------------------------
-- library declaration
library IEEE;
use IEEE.std_logic_1164.all;
-- entity
entity D5M_clock is
	port ( CLOCK_50 : in std_logic;
			D5M_XCLKIN : out std_logic);
end D5M_clock;
-- architecture
architecture myD5M_clock of D5M_clock is
begin
	D5M_XCLKIN <= CLOCK_50; -- put 50Mhz clock on the clock input of the camera
end myD5M_clock;

---------------------------------
-- Data feeder
---------------------------------
-- library declaration
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
-- entity
entity datafeeder is
	port ( feed_clk : in std_logic; 
			i2c_busy : in std_logic;
			i2c_data_rd : in std_logic_vector(7 downto 0);
			feed_rst_n : in std_logic;
			--------------------------------
			i2c_reset_n : out std_logic;
			i2c_ena : out std_logic;
			i2c_addr : out std_logic_vector(6 downto 0);
			i2c_rw : out std_logic;
			i2c_data_wr : out std_logic_vector(7 downto 0);
			data : out std_logic_vector (15 downto 0));
end datafeeder;
-- architecture
architecture my_datafeeder of datafeeder is
	signal busy_prev : std_logic;
	constant slave_addr : std_logic_vector(6 downto 0) := "1011101"; -- The address of d5m camera is BA write and BB read, the first 7 bits are 1011101 or 5D
	type state_type is (start,getcmd,delay,writereg,readreg,donewr,done); -- All the states of the FSM
	signal state : state_type;
	subtype Cmd_t is std_logic_vector (23 downto 0);
	type Commands_t is array (natural range <>) of Cmd_t;
	constant Commands : Commands_t := (
	-- 8 MSB bits for register address and 16 LSB bits for data
	x"0B" & x"0003", --Restart, pause acquisition before changing settings
	-- Change pixclock to 96MHz (Nope. Max Throughput of pipeline is at 50Mhz for 320x240 picture on LCD
	--x"10" & x"0051", --PLL control, (power on : last bit = 1)
	--x"11" & x"6018", --PLL config 1, (m factor : 96, n factor - 1 : 24)
	--x"12" & x"0001", --PLL config 2, (p factor - 1 : 1)
	--x"10" & x"0053", --PLL control, (power on and enable on, last 2 bits)
	-- Change image size to 320x240
	x"01" & x"0042", --Row start, add 12 to default 54 = 66
	x"02" & x"0020", --Column start, add 16 to default 16 = 32
	x"03" & x"077F", --Row size, 240*8 - 1 = 1919
	x"04" & x"09FF", --Column size, 320*8 -1 = 2559
	x"22" & x"0003", --Row address mode, bin1, skip4, skip 4 pour une image de 640x480 en pattern bayer
	x"23" & x"0003", --Column address mode, bin1, skip4, le bin rajoute du flou en déplacement
	x"0B" & x"0001");--Restart, clear pause_restart bit
	signal Index : integer range 0 to Commands'length; -- Index of the command set
	signal Cmd : Cmd_t;
	signal reg_addr : std_logic_vector(7 downto 0);
	signal reg_data : std_logic_vector(15 downto 0);
	signal rw : std_logic;

begin	
	state_machine : process(feed_clk,feed_rst_n)
	variable busy_cnt : integer range 0 to 3;
	variable clockcount : integer range 0 to 49999 := 49999; -- Delay 1ms between each command
	begin
		if (feed_rst_n = '0') then
			i2c_reset_n <= '0';
			Index <= 0;
			rw <= '0';
			state <= start;
		elsif rising_edge(feed_clk) then
			Cmd <= Commands(Index);
			case state is
				when start =>
					i2c_reset_n <= '1';
					i2c_ena <= '0';
					state <= getcmd;
				
				when getcmd =>
					reg_addr <= Cmd(23 downto 16);
					reg_data <= Cmd(15 downto 0);
					state <= delay;
					
				when delay =>
					if rw = '0' then
						if clockcount = 0 then
							state <= writereg;
						end if;
						clockcount := clockcount - 1;
					else
						state <= readreg;
					end if;
					 
				when writereg =>                               --state for conducting this transaction
					busy_prev <= i2c_busy;                       --capture the value of the previous i2c busy signal
					if(busy_prev = '0' AND i2c_busy = '1') then  --i2c busy just went high
						busy_cnt := busy_cnt + 1;                    --counts the times busy has gone from low to high during transaction
					end if;
					case busy_cnt is                             --busy_cnt keeps track of which command we are on
						when 0 =>                                  --no command latched in yet
							i2c_ena <= '1';                            --initiate the transaction
							i2c_addr <= slave_addr;                    --set the address of the slave
							i2c_rw <= '0';                             --command 1 is a write
							i2c_data_wr <= reg_addr;           			--write register address
						when 1 =>
							i2c_rw <= '0';										--command 2 is a write
							i2c_data_wr <= reg_data(15 downto 8); 		--write msb bits of data to register
						when 2 =>
							i2c_data_wr <= reg_data(7 downto 0);		--write lsb bits of data to register
						when 3 =>
							i2c_ena <='0';										--disable i2c module, all commands are passed to latch
							if (i2c_busy = '0') then						--i2c module has finihsed
								busy_cnt := 0;									--reset busy counter
								state <= done;
							end if;
						when others => null;
					end case;
					
				when readreg =>
					busy_prev <= i2c_busy;                       --capture the value of the previous i2c busy signal
					if(busy_prev = '0' AND i2c_busy = '1') then  --i2c busy just went high
						busy_cnt := busy_cnt + 1;                    --counts the times busy has gone from low to high during transaction
					end if;
					case busy_cnt is
						when 0 =>                                  --no command latched in yet
							i2c_ena <= '1';                            --initiate the transaction
							i2c_addr <= slave_addr;                    --set the address of the slave
							i2c_rw <= '0';                             --command 1 is a write
							i2c_data_wr <= reg_addr;              --data to be written
						when 1 =>
							i2c_rw <= '1';
						when 2 =>
							i2c_rw <= '1';
							if (i2c_busy = '0') then
								data(15 downto 8) <= i2c_data_rd;
							end if;
						when 3 =>
							i2c_ena <= '0';
							if (i2c_busy = '0') then
								data(7 downto 0) <= i2c_data_rd;
								busy_cnt := 0;
								state <= done;
							end if;
						when others => null;
					end case;

				when done =>
					if Index=Commands'length then -- last command has been sent
						--state <= donewr;
						state <= done;
						data <= x"FF00";
					else
						Index <= Index + 1; -- fecth next vector
						state <= getcmd;
					end if;
				
				when donewr => -- read in loop all registers written
					rw <= '1';
					Index <= 0;
					state <= getcmd;
					
			end case;
		end if;
	end process state_machine;
end my_datafeeder;


---------------------
-- Pixel Interfacing
---------------------
-- library declaration
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
-- entity
entity pixel_interface is
	port ( D5M_PIXCLK : in std_logic;
			D5M_LVAL : in std_logic;
			D5M_FVAL : in std_logic;
			D5M_Di : in std_logic_vector(11 downto 0);
			---------------------------------------------
			--pixel : out std_logic_vector(11 downto 0));
			pixel_out : out std_logic_vector(4 downto 0);
			Xpos_out : out unsigned(11 downto 0);
			Ypos_out : out unsigned(10 downto 0));
end pixel_interface;
-- architecture
architecture my_pixel_interface of pixel_interface is
	signal Xpos : unsigned(11 downto 0);
	signal Ypos : unsigned(10 downto 0);
	signal newline : std_logic;
begin
	pixel_latch: process(D5M_PIXCLK,D5M_LVAL,D5M_FVAL) -- latch pixel data
	begin
		if (falling_edge(D5M_PIXCLK) and (D5M_LVAL = '1') and (D5M_FVAL = '1')) then -- read pixel data when it's valid
			pixel_out <= D5M_Di(8 downto 4);
		end if;
	end process pixel_latch;
	
	position_counter : process(D5M_FVAL,D5M_LVAL,D5M_PIXCLK)
	begin
		if (D5M_FVAL = '0') then
			Ypos <= to_unsigned(-1, Ypos'length); -- -1 to start the first count at 0
		elsif (D5M_LVAL = '0') then
			Xpos <= to_unsigned(-1, Xpos'length); -- Reset Xpos counter (-1 to start the first count at 0)
			newline <= '1';
		elsif (falling_edge(D5M_PIXCLK)) then -- When a new valid pixel is read
			if (newline = '1') then -- Increment Ypos count only once a new line
				Ypos <= Ypos + 1; 
				newline <= '0';
			end if;
			Xpos <= Xpos + 1; -- Increment Xpos count
		end if;
	end process position_counter;
	
	Xpos_out <= Xpos; -- Final output assignement
	Ypos_out <= Ypos;
end my_pixel_interface;


-----------------------
-- Image transformation
-----------------------
-- library declaration
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
-- entity
entity image_transform is
	port( D5M_PIXCLK : in std_logic;
			pixel_in : in std_logic_vector(4 downto 0);
			Xpos_in : in unsigned(11 downto 0);
			Ypos_in : in unsigned(10 downto 0);
			q_in : in std_logic_vector(4 downto 0);
			-----------------------------------
			data_out : out std_logic_vector(4 downto 0);
			rdclk_out : out std_logic;
			rdreq_out : out std_logic;
			wrclk_out : out std_logic;
			wrreq_out : out std_logic;
			pixImage_out : out std_logic_vector(15 downto 0);
			newpixclk_out : out std_logic);
end image_transform;
-- architecture
architecture my_image_transform of image_transform is
	signal Xposchosen : unsigned(11 downto 0);
	signal Yposchosen : unsigned(10 downto 0);
	
begin
	chosepix : process(Xpos_in,Ypos_in)
	begin
		if ((Xpos_in < 640) and (Ypos_in < 480)) then -- for a 320x240 picture
			Xposchosen <= Xpos_in mod 2; --Read bayer pattern (skip 1 col)
			Yposchosen <= Ypos_in mod 2; --Read bayer pattern (skip 1 row)
		else 
			Xposchosen <= (others=>'1');
			Yposchosen <= (others=>'1');
		end if;
	end process chosepix;
	
	wrclk_out <= not D5M_PIXCLK; -- write and read fifo at falling edge of the clock
	rdclk_out <= not D5M_PIXCLK;
	
	pixelsave : process(D5M_PIXCLK,Yposchosen,Xposchosen) -- save first row each 8 rows and skip 8 pixels on x axis to match output resolution
	begin
		if ((Yposchosen = 0) and (Xposchosen = 1)) then
			if (rising_edge(D5M_PIXCLK) ) then
				wrreq_out <= '1';
				data_out <= pixel_in; -- put the first pixel row in memory buffer
			end if;
		else
			wrreq_out <= '0';
		end if;
	end process pixelsave;
	
	pixelmix : process(D5M_PIXCLK,Yposchosen,Xposchosen,q_in)
	begin
		if (rising_edge(D5M_PIXCLK) and (Yposchosen = 1) and (Xposchosen = 0)) then
				pixImage_out(4 downto 0) <= pixel_in; -- blue value of output pixel
		end if;
		if ((Yposchosen = 1) and (Xposchosen = 1)) then
			if (rising_edge(D5M_PIXCLK)) then
				rdreq_out <= '1';
				pixImage_out(10 downto 5) <= pixel_in & '0'; -- green value of output pixel
			end if;
		else
			pixImage_out(15 downto 11) <= q_in; -- red value of output pixel
			rdreq_out <= '0';
		end if;
	end process pixelmix;
	
	pixclckgen : process(D5M_PIXCLK,Xposchosen,Yposchosen) -- generates a clock at the pixel_out rate
	begin
		if (rising_edge(D5M_PIXCLK) and (Xposchosen < 1) and (Yposchosen = 1)) then
			newpixclk_out <= '0';
		elsif (rising_edge(D5M_PIXCLK) and (Xposchosen >= 1) and (Yposchosen = 1)) then
			newpixclk_out <= '1';
		end if;
	end process pixclckgen;
end my_image_transform;


-----------------
-- LCD controller
-----------------
-- library declaration
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
-- entity
entity lcd_controller is
	port ( Clk : in std_logic;
			Rst : in std_logic;
			pixImage_in : in std_logic_vector(15 downto 0);
			newpixclk_in : in std_logic;
			--------------------
			-- LT24 graphic Controller
			LT_DBi : out std_logic_vector(15 downto 0); -- we use it as OUT only
			LT_WR_n : out std_logic; -- aka WRX
			LT_RD_n : out std_logic; -- aka RDX
			LT_RS : out std_logic; -- aka D/CX
			LT_CS_n : out std_logic; -- aka CSX
			LT_RESET_n : out std_logic; -- aka RESX
			LT_LCD_ON : out std_logic; -- Transistor to drive LCD lighting (1 = on)
			-- D5M "flow control"
			D5M_RESET_n : out std_logic;
			setcamconfig : out std_logic);
			
end lcd_controller;
-- architecture
architecture my_lcdctrl of lcd_controller is
	subtype Cmd_t is std_logic_vector (28 downto 0);
	-- Dly_ms(12) & C/D & Data(16) - 29 bits, could be a record
	type Commands_t is array (natural range <>) of Cmd_t;
	constant Commands : Commands_t := (
	-- delay_ms D/Cd_n DB -- p.83 datasheet ILI9341
	x"001" & '0' & x"0011", -- Exit Sleep
	-- x"100" & '0' & x"0001", -- Software reset
	x"800" & '0' & x"0029", -- Display ON
	x"001" & '0' & x"003A", -- Set COLMOD register
	x"000" & '1' & x"0055", -- non-default 16 bits RGB data
	
	x"000" & '0' & x"0036", -- Memory Access Control
	--x"000" & '1' & x"0008", -- non-default BGR filter !
	x"000" & '1' & x"0028", -- non-default BGR filter + row/col inversion for putting the display in 320x240 mode
	x"000" & '0' & x"002A", -- Column adress set
	x"000" & '1' & x"0000",
	x"000" & '1' & x"0000",
	x"000" & '1' & x"0001",
	x"000" & '1' & x"003F", -- Put max column into max page, because of row/col inversion
	x"000" & '0' & x"002B", -- Page adress set
	x"000" & '1' & x"0000",
	x"000" & '1' & x"0000",
	x"000" & '1' & x"0000",
	x"000" & '1' & x"00EF", -- Put max page into max column, because of row/col inversion
	
	x"000" & '0' & x"00F2", -- Enable 3G register
	x"000" & '1' & x"0000", -- non-default = disable 3 gamma -- à voir plus tard si je veux une correction gamma ou pas, pour l'instant on va laisser ça comme ça
	x"000" & '0' & x"002C"); -- Memory Write
	
	type state_type is (Boot,Idle,GetCmd,Dly,Wr1,Wr2,Done,Done1,CamLiveFeed); -- All the states of the FSM
	signal State : state_type;
	signal Index : integer range 0 to Commands'length; -- Index of the command set
	signal Cntr : integer range 0 to to_integer(x"FFF"); -- Count of delay between instructions
	signal Cycle : integer range 0 to 255; -- Counter for the write cycle
	signal Cmd : Cmd_t;
	constant Nsetup : integer := 5; -- Write cycle, try to reduce it to speed up
	constant Nhold : integer := 5;
	signal Tick1ms : std_logic;

-- ---------------------------
-- ILI9341 interface
-- ---------------------------
-- CSX can be kept low
-- Write cycle > 66ns = 100 ns (5c = 2 + 3) can work
-- Tdst data setup > 10 ns
-- Tdht data hold > 10 ns
-- /!\ Data read (not used here) is SLOW (500 ns)
begin
	LT_RD_n <= '1'; -- we don't read

	tickclk : process(Clk)
	variable clockcount : integer range 0 to 49999 := 49999;
	begin
		if falling_edge(Clk) then
			if clockcount = 0 then
				clockcount := 49999;
				Tick1ms <= '1';
			else
				clockcount := clockcount - 1;
				Tick1ms <= '0';
			end if;
		end if;
	end process tickclk;


	state_machine : process(Clk,Rst)
	begin
		if Rst='0' then
			State <= Boot;
			LT_DBi <= (others=>'0');
			LT_WR_n <= '1';
			LT_CS_n <= '1';
			LT_RS <= '0';
			LT_RESET_n <= '0';
			LT_LCD_ON <= '0';
			Index <= 0;
			Cntr <= 15;
			Cycle <= 0;
			Cmd <= (others=>'0');
			D5M_RESET_n <= '0'; -- Put D5M camera on hold while LCD is reset
			setcamconfig <= '0';
		
		elsif rising_edge(Clk) then
			Cmd <= Commands(Index);
			case State is
				
				when Boot =>
					LT_RESET_n <= '0';
					Index <= 0;
					if Tick1ms='1' then
						if Cntr=0 then
							LT_RESET_n <= '1';
							Cntr <= 300;
							State <= Idle;
						else
							Cntr <= Cntr - 1;
						end if;
					end if;
	 
				when Idle =>
					if Cntr=0 then
						LT_LCD_ON <= '1'; -- Light ON
						LT_CS_n <= '0';
						State <= GetCmd;
					elsif Tick1ms='1' then
						Cntr <= Cntr - 1;
					end if;
	 
				when GetCmd =>
					Cntr <= to_integer(unsigned(Cmd(Cmd_t'high downto Cmd'high-11)));
					LT_DBi <= Cmd(LT_DBi'range);
					LT_RS <= Cmd(LT_DBi'length);
					State <= Dly;
	 
				when Dly =>
					if Cntr = 0 then
						Cycle <= Nsetup-1;
						LT_WR_n <= '0';
						State <= Wr1;
					elsif Tick1ms='1' then
						Cntr <= Cntr-1;
					end if;
	 
				when Wr1 => -- Setup
					if Cycle = 0 then
						Cycle <= Nhold-1;
						LT_WR_n <= '1'; -- rising edge (trig write)
						State <= Wr2;
					else
						Cycle <= Cycle-1;
					end if;
	 
				when Wr2 => -- Hold
					if Cycle = 0 then
						State <= Done;
					else
						Cycle <= Cycle-1;
					end if;
	
				when Done =>
					if Index=Commands'length-1 then -- last command has been sent
						LT_RS <= '1'; -- Put the LT interface in data mode
						D5M_RESET_n <= '1'; -- enable D5M camera
						setcamconfig <= '1'; -- config D5M camera trough i2c
						State <= CamLiveFeed;
					else
						Index <= Index + 1; -- fecth next vector
						State <= Done1;
					end if;
	 
				when Done1 => -- compensate double pipeline
					State <= GetCmd;
					
				when CamLiveFeed =>
					LT_DBi <= pixImage_in; -- Write pixel to LCD data port
					LT_WR_n <= not newpixclk_in; -- toggle write pin with pixel clock (pour une raison obscure, il y a un délai quelque part, donc il faut rajouter not pour que les pixels s'écrivent au bon moment
					State <= CamLiveFeed; -- deadlock
	 
			end case;
		end if;
	end process state_machine;
end my_lcdctrl;
	
			