-define(IMAP_PORT, 143).
-define(IMAPS_PORT, 993).

-record(imapc_fsm,{
	socket = [],
	state = not_authenticated,  % [not_authenticated,authenticated,selected,logout]
	capability = [],
	mailbox = [],
	mail = [],
	encrypt = plain,
	peer = []
	}).

-record(imap_resp,{
	pid       = [], % needed for IMAP response server
	mailbox   = [], % needed for IMAP response server
	timestamp = [], % needed for IMAP response server
	tag       = [],
	status    = [], % OK, NO, BAD, PREAUTH, BYE
	code      = [], % ALERT, BADCHARSET, CAPABILITY, PARSE, PERMANENTFLAGS, READ_ONLY, READ_WRITE, TRYCREATE, UIDNEXT, UIDVALIDITY, UNSEEN - found inside []
	data      = [],
	cmd       = [],
	info      = []
	}).

-record(mailbox,{
	name        = [],
	flags       = [],
	permflags   = [],
	exists      = 0,
	messages    = 0,
	recent      = 0,
	unseen      = 0,
	uidvalidity = 0,
	uidnext     = 0,
	myrights    = [],
	readwrite   = false
	}).


-record(address,{
	addr_name    = [],
	addr_adl     = [],
	addr_mailbox = [],
	addr_host    = []
	}).

-record(envelope,{
	date = [],
	subject = [],
	from = [],
	sender = [],
	reply_to = [],
	to = [],
	cc = [],
	bcc = [],
	in_reply_to = [],
	message_id = []
	}).

-record(fetch,{
	seqnum = 0,
	uid = [],
	size = 0,
	flags = [],
	internaldate = [],
	date = [],
	subject = [],
	from = [],
	sender = [],
	reply_to = [],
	to = [],
	cc = [],
	bcc = [],
	in_reply_to = [],
	message_id = [],
	body = [],
	body_structure = [],
	rfc822 = [],
	rfc822_header = [],
	rfc822_text = [],
	parts = []
	}).

-record(body,{
	type = [],
	parts = [],
	md5 = [],
	dsp = [],
	lang = [],
	loc = []
	}).

-record(part,{
	type = [],
	subtype = [],
	params = [],
	id = [],
	desc = [],
	encoding = [],
	octets = [],
	lines = [],
	md5 = [],
	dsp = [],
	lang = [],
	loc = []
	}).

-record(folder,{
	name = [],
	delim = ".",
	flags = []
	}).