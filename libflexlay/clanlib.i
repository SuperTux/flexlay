%include "std_string.i"

class CL_Component
{
private:
	~CL_Component();
public:
	CL_Component(CL_Component* parent, CL_StyleManager* style = NULL);
        void show(bool show = true);
       	bool is_visible(bool check_parents = true);
        void set_focus();
	void set_size(int new_width, int new_height);
	void set_position(int new_x, int new_y);
	const CL_Rect& get_position();

	int get_width() const;
	int get_height() const;

	int get_screen_x() const;
	int get_screen_y() const;

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
	const std::string &get_text() const;
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

class CL_InputBox : public CL_Component
{
public:
	CL_InputBox(
		const CL_Rect &pos,
		CL_Component *parent,
		CL_StyleManager *style = NULL);

	CL_Signal_v0& sig_return_pressed();

	void set_text(const std::string &text);
	const std::string &get_text() const;        
};

class CL_Label : public CL_Component
{
public:
		//: Label Constructor
	CL_Label(
		const CL_Point &pos,
		const std::string &text,
		CL_Component *parent,
		CL_StyleManager *style = NULL);
};

class CL_ListBox : public CL_Component
{
public:
	CL_ListBox(     
		const CL_Rect& pos, CL_Component* parent, CL_StyleManager* style = NULL);
       	CL_Signal_v1<int>& sig_highlighted();   
	int insert_item(
		const std::string& text, int index = -1);
};

class CL_ResourceManager
{
public:
	CL_ResourceManager();
	CL_ResourceManager(
		const std::string &config_file,
		CL_InputSourceProvider *provider = 0,
		bool delete_inputsource_provider = false);
	~CL_ResourceManager();

	std::vector<std::string> get_all_resources(const std::string &section_name);
	std::vector<std::string> get_all_sections();

	std::vector<std::string> get_resources_of_type(const std::string &type_id);
	std::vector<std::string> get_resources_of_type(const std::string &type_id, const std::string &section_name);
	void add_resources(const CL_ResourceManager &additional_resources);
	void remove_resources(const CL_ResourceManager &additional_resources);
};


class CL_RadioButton : public CL_Button
{
public:
	CL_RadioButton(
		const CL_Point &pos,
		const std::string &text,
		CL_Component *parent,
		CL_StyleManager *style = NULL);
	bool is_checked() const;
	void set_checked(bool check);
};

class CL_RadioGroup
{
public:
	CL_RadioGroup();
	~CL_RadioGroup();

	const std::vector<CL_RadioButton *> &get_buttons() const;
	void add(CL_RadioButton *button, bool delete_component = false);
};

class CL_CheckBox : public CL_Button
{
public:
        CL_CheckBox(
		const CL_Point &pos,
		const std::string &text,
		CL_Component *parent,
		CL_StyleManager *style = NULL);
	bool is_checked() const;
	void set_checked(bool check = true);
};

class CL_ProviderFactory
{
public:
	static CL_PixelBuffer load(
		const std::string &filename,
		const std::string &type = "",
		CL_InputSourceProvider *input_provider = 0);

	static void save(
		CL_PixelBuffer buffer,
		const std::string &filename,
		const std::string &type = "",
		CL_OutputSourceProvider *output_provider = 0);
};


/* EOF */

