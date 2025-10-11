// @ts-check

import mdx from '@astrojs/mdx';
import sitemap from '@astrojs/sitemap';
import { defineConfig } from 'astro/config';

export default defineConfig({
	integrations: [mdx(), sitemap()],
	markdown: {
		shikiConfig: {
			themes: {
				dark: 'catppuccin-macchiato',
				light: 'catppuccin-latte'
			}
		}
	},
	site: 'https://ignamartinoli.github.io'
});
