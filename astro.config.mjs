// @ts-check

import mdx from '@astrojs/mdx';
import sitemap from '@astrojs/sitemap';
import { defineConfig } from 'astro/config';

export default defineConfig({
	integrations: [mdx(), sitemap()],
	markdown: {
		shikiConfig: {
			theme: 'catppuccin-macchiato'
		}
	},
	site: 'https://example.com'
});
