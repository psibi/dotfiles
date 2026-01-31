(use-package gptel
  :init
  :custom
  (gptel-api-key #'gptel-api-key-from-auth-source)
  (gptel-model 'gemini-3-pro-preview)
  (gptel-include-reasoning nil)
  (gptel-default-mode 'markdown-mode)
  :bind
  (:map global-map
        ("C-c l" . gptel-send))
  :config
  (setq gptel-backend (gptel-make-gemini "Gemini"
			:key (plist-get (car (auth-source-search :host "localhost.gemini-paid"))
					:secret)
			:stream t))
  (gptel-make-preset 'english-reader
  :description "Preset for better sentences"
  :backend "Gemini"
  :model 'gemini-3-pro-preview
  :system "Act as an expert English proofreader. Please review the text. Correct any spelling and grammar errors, and improve the clarity and flow. Make sure your changes are appropriate for the context.")
  (gptel-make-preset 'code-reader
  :description "Preset for code reader"
  :backend "Gemini"
  :model 'gemini-3-pro-preview
  :system "Act as an expert programmer and senior code reviewer with deep knowledge of software architecture, best practices, and performance. Your task is to provide a clear and comprehensive explanation of the code I provide.")

)

(use-package llm
  :quelpa (llm :fetcher github
	       :repo "ahyatt/llm")
  :init
  (require 'llm-gemini)
  :custom
  (llm-warn-on-nonfree nil))

(use-package magit-gptcommit
  :after (magit llm)
  :quelpa (magit-gptcommit :fetcher github
			   :repo "douo/magit-gptcommit")

  :config
  (magit-gptcommit-status-buffer-setup)
  :custom
  (magit-gptcommit-llm-provider
   (make-llm-gemini
    :key (plist-get (car (auth-source-search :host "localhost.gemini-free"))
		    :secret)
    ;; :chat-model "gemini/gemini-2.5-pro")
    :chat-model "gemini-3-flash-preview")
   )
  (magit-gptcommit-prompt "You are an expert programmer writing a commit message.
You went over every file diff that was changed in it.

First Determine the best label for the diffs.
Here are the labels you can choose from:
- build: Changes that affect the build system or external dependencies (example scopes: gulp, broccoli, npm)
- chore: Updating libraries, copyrights or other repo setting, includes updating dependencies.
- ci: Changes to our CI configuration files and scripts (example scopes: Travis, Circle, GitHub Actions)
- docs: Non-code changes, such as fixing typos or adding new documentation
- feat: a commit of the type feat introduces a new feature to the codebase
- fix: A commit of the type fix patches a bug in your codebase
- perf: A code change that improves performance
- refactor: A code change that neither fixes a bug nor adds a feature
- style: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
- test: Adding missing tests or correcting existing tests

Then summarize the commit into a single specific and cohesive
theme.  Remember to write in only one line, no more than 50
characters.  Write your response using the imperative tense
following the kernel git commit style guide.  Write a high level
title followed by a summary of the high level of changes in a
couple of paragraphs. If required, also addd a list of bullet
points with details about the changes. Make sure that a single
bullet point also doesn't exceed more than 70 characters.

THE FILE DIFFS:
```
%s
```
Now write Commit message in follow template: [label]:[one line of summary] :

Paragraph describing the change. Make sure to wrap the paragraph such
that it does not exceed 72 characters per line.

list of bullet points.
"
))

(use-package gptel-quick
  :after gptel
  :quelpa (gptel-quick :fetcher github
		       :repo "karthink/gptel-quick")
  :ensure t)
