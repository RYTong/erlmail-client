mail_client中imap协议使用介绍
=========================
mail_client中的imap客户端功能是基于[boorad/erlimap](https://github.com/boorad/erlimap)的
代码开发的，在其基础上添加了LIST, COPY, APPEND, EXPUNGE等命令的实现，同时对服务器响应数据
做了简单封装，方便APP处理。

imap命令接口位于imapc模块中，大多数客户端操作都可以通过对相关命令接口的组合使用来完成。同时
mail_client也封装了一些常用操作的接口，当然，这些接口也全是对imapc中命令接口的一层封装而
已。

通过mail_client中的imap接口来完成一组imap客户端操作示例
------------------------------------------------------------------------------------------------
    %% 打开一个imap账户连接
    {ok, Pid} = imapc:open_account(ssl, "imap.mail.yahoo.com", 993, "username", "password")
    列出所有邮箱信息，包括邮件总数，未读邮件数
    {ok, Mailboxex} = mail_client:imap_list_mailbox(P),
    %% 选中收件箱，并列出最近5条邮件的Subject, From, Date及是否有附件信息
    {ok, {Mailbox, Mails}} = mail_client:imap_select_mailbox(Pid, "\\Inbox", 5),
    %% 列出从15条至23条邮件简要信息
    {ok, Mails} = mail_client:imap_list_message(Pid, 15, 23),
    %% 获取第20条邮件内容
    {ok, Mail} = mail_client:imap_retrieve_message(Pid, 20),
    %% 设置第20条邮件为已读
    mail_client:imap_seen_message(Pid, 20),
    %% 将该邮件移到回收站
    mail_client:imap_trash_message(Pid, 20),
    %% 清空当前邮件箱标记为\Deleted的邮件
    mail_client:imap_clear_mailbox(Pid),
    %% 退出邮箱
    imapc:close_account(Pid).
相应也提供有移动邮件接口`mail_client:imap_move_message/3`，保存草稿邮件接口
`mail_client:imap_save_draft/2`。
    
    
