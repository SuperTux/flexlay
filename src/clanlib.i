class CL_Size
{
public:
	CL_Size(int w, int h);
	int width;
	int height;
};

class CL_Point
{
public:
	CL_Point(int x, int y)
		: x(x), y(y) { }
	int x;
	int y;
};

class CL_Rect
{
public:
	int left;
	int right;
	int top;
	int bottom;

	CL_Rect(int, int, int, int);
	CL_Rect(const CL_Point&, const CL_Size&);     

	int get_width() const;
	int get_height() const;
};

class CL_Color
{
public:
	CL_Color(unsigned int, unsigned int, unsigned int, unsigned int);

	void set_red  (unsigned int);
	void set_blue (unsigned int);
	void set_green(unsigned int);
	void set_alpha(unsigned int);

	unsigned int get_red  ();
	unsigned int get_blue ();
	unsigned int get_green();
	unsigned int get_alpha();
};

class CL_Component
{
private:
	~CL_Component();
public:
	CL_Component(CL_Component* parent, CL_StyleManager* style = NULL);
};

class CL_Window : public CL_Component
{
private:
	~CL_Window();
public:
	CL_Window(
		const CL_Rect &pos,
		const std::string &title,
		CL_Component *parent,
		CL_StyleManager *style = NULL);
	CL_Component* get_client_area();
};

class CL_Button : public CL_Component
{
private:
	~CL_Button();
public:	
	CL_Button(
		const CL_Rect &pos,
		const std::string &text,
		CL_Component *parent,
		CL_StyleManager *style = NULL);

	CL_Signal_v0 &sig_clicked();
};

class CL_Menu : public CL_Component
{
private:
	~CL_Menu();
public:
	CL_Menu(
		const CL_Rect &rect,
		CL_Component *parent,
		CL_StyleManager *style = NULL,
		bool vertical=false);

	CL_Menu(
		CL_Component *parent,
		CL_StyleManager *style = NULL,
		bool vertical=false);
	
	CL_MenuNode *create_item( const std::string &path, const std::string &labels=std::string());
};

class CL_MenuNode : public CL_Component
{
private:
	~CL_MenuNode();
//! Construction:
public:
	//: CL_MenuNode Constructor
	CL_MenuNode(
		CL_Menu *parent_menu,
		CL_StyleManager *style = NULL);
	CL_Signal_v0 &sig_clicked();
};

/* EOF */
